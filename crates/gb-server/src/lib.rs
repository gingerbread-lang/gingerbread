use ast::validation::ValidationDiagnostic;
use ast::AstNode;
use line_index::{ColNr, LineIndex, LineNr};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, Position, Range, SelectionRange, SemanticToken,
    SemanticTokenModifier, SemanticTokenType, TextDocumentContentChangeEvent, Url,
};
use parser::Parse;
use std::collections::HashMap;
use std::mem;
use std::ops::BitOrAssign;
use syntax::{SyntaxElement, SyntaxKind};
use text_size::{TextRange, TextSize};

#[derive(Default)]
pub struct GlobalState {
    world_index: hir::WorldIndex,
    analyses: HashMap<Url, Analysis>,
}

struct Analysis {
    content: String,
    line_index: LineIndex,
    module_name: hir::Name,
    parse: Parse,
    ast: ast::Root,
    validation_diagnostics: Vec<ValidationDiagnostic>,
    index: hir::Index,
    indexing_diagnostics: Vec<hir::IndexingDiagnostic>,
    bodies: hir::Bodies,
    lowering_diagnostics: Vec<hir::LoweringDiagnostic>,
    inference_result: hir_ty::InferenceResult,
    ty_diagnostics: Vec<hir_ty::TyDiagnostic>,
}

impl GlobalState {
    pub fn open_file(&mut self, uri: Url, content: String) {
        let filename = uri.path_segments().unwrap().next_back().unwrap();
        let module_name = filename.find('.').map_or(filename, |dot| &filename[..dot]);

        let analysis =
            Analysis::new(content, hir::Name(module_name.to_string()), &mut self.world_index);

        for analysis in self.analyses.values_mut() {
            analysis.recheck(&mut self.world_index);
        }

        self.analyses.insert(uri, analysis);
    }

    pub fn apply_changes(&mut self, uri: &Url, changes: Vec<TextDocumentContentChangeEvent>) {
        self.analyses.get_mut(uri).unwrap().apply_changes(changes, &mut self.world_index);

        for (analysis_uri, analysis) in &mut self.analyses {
            if analysis_uri == uri {
                continue;
            }

            analysis.recheck(&mut self.world_index);
        }
    }

    pub fn extend_selection(&mut self, uri: &Url, position: Position) -> SelectionRange {
        self.analyses[uri].extend_selection(position)
    }

    pub fn highlight(&self, uri: &Url) -> Vec<SemanticToken> {
        self.analyses[uri].highlight()
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = (&Url, Vec<Diagnostic>)> {
        self.analyses.iter().map(|(uri, analysis)| (uri, analysis.diagnostics()))
    }
}

impl Analysis {
    fn new(content: String, module_name: hir::Name, world_index: &mut hir::WorldIndex) -> Self {
        let parse = {
            let tokens = lexer::lex(&content);
            parser::parse_source_file(&tokens, &content)
        };
        let tree = parse.syntax_tree();
        let ast = ast::Root::cast(tree.root(), tree).unwrap();
        let (index, indexing_diagnostics) = hir::index(ast, tree, world_index);
        let (bodies, lowering_diagnostics) = hir::lower(ast, tree, &index, world_index);
        let (inference_results, ty_diagnostics) = hir_ty::infer_all(&bodies, &index, world_index);

        world_index.add_module(module_name.clone(), index.clone());

        let mut analysis = Self {
            content,
            line_index: LineIndex::default(),
            module_name,
            parse,
            ast,
            validation_diagnostics: Vec::new(),
            index,
            indexing_diagnostics,
            bodies,
            lowering_diagnostics,
            inference_result: inference_results,
            ty_diagnostics,
        };

        analysis.update_line_index();
        analysis.validate();

        analysis
    }

    fn apply_changes(
        &mut self,
        changes: Vec<TextDocumentContentChangeEvent>,
        world_index: &mut hir::WorldIndex,
    ) {
        for change in changes {
            let range = match change.range {
                Some(r) => convert_lsp_range(r, &self.line_index),
                None => {
                    // if no range is present, the text is considered
                    // the new full content of the document.
                    self.content = change.text;
                    continue;
                }
            };

            let start_idx = usize::from(range.start());
            let end_idx = usize::from(range.end());
            self.content.replace_range(start_idx..end_idx, &change.text);

            self.update_line_index();
        }

        self.reparse();
        self.validate();
        self.index(world_index);
        self.recheck(world_index);
    }

    fn recheck(&mut self, world_index: &mut hir::WorldIndex) {
        self.lower(world_index);
        world_index.update_module(&self.module_name, self.index.clone());

        self.infer(world_index);
    }

    fn extend_selection(&self, position: Position) -> SelectionRange {
        let offset = convert_lsp_position(position, &self.line_index);

        let mut ranges = vec![self.ast.range(self.parse.syntax_tree())];
        let mut last_node = self.ast.syntax();

        let mut push_range = |range| {
            if ranges[ranges.len() - 1] != range {
                ranges.push(range);
            }
        };

        for node in self.ast.syntax().descendant_nodes(self.parse.syntax_tree()) {
            let range = node.range(self.parse.syntax_tree());
            if range.contains_inclusive(offset) {
                last_node = node;
                push_range(range);
            }
        }

        for token in last_node.child_tokens(self.parse.syntax_tree()) {
            let range = token.range(self.parse.syntax_tree());

            if token.kind(self.parse.syntax_tree()) == SyntaxKind::Whitespace {
                continue;
            }

            if range.contains_inclusive(offset) {
                push_range(range);
                break;
            }
        }

        let root = ranges[0];
        let mut range =
            SelectionRange { range: convert_text_range(root, &self.line_index), parent: None };

        for r in ranges.into_iter().skip(1) {
            range = SelectionRange {
                range: convert_text_range(r, &self.line_index),
                parent: Some(Box::new(range)),
            };
        }

        range
    }

    fn highlight(&self) -> Vec<SemanticToken> {
        let mut tokens = Vec::new();
        let mut prev_token_position = None;
        let mut last_parent_node_kind = SyntaxKind::Root;

        for element in self.ast.syntax().descendants(self.parse.syntax_tree()) {
            let token = match element {
                SyntaxElement::Node(node) => {
                    last_parent_node_kind = node.kind(self.parse.syntax_tree());
                    continue;
                }
                SyntaxElement::Token(token) => token,
            };

            let mut modifiers = HighlightModifiers(0);

            let token_type = {
                let hi = match token.kind(self.parse.syntax_tree()) {
                    SyntaxKind::LetKw | SyntaxKind::FncKw => HighlightKind::Keyword,
                    SyntaxKind::Int => HighlightKind::Number,
                    SyntaxKind::String => HighlightKind::String,
                    SyntaxKind::Plus
                    | SyntaxKind::Hyphen
                    | SyntaxKind::Asterisk
                    | SyntaxKind::Slash => HighlightKind::Operator,
                    SyntaxKind::Comment => HighlightKind::Comment,

                    SyntaxKind::Ident => match last_parent_node_kind {
                        SyntaxKind::LocalDef => {
                            modifiers |= HighlightModifier::Declaration;
                            HighlightKind::Local
                        }
                        SyntaxKind::Param => {
                            modifiers |= HighlightModifier::Declaration;
                            HighlightKind::Param
                        }
                        SyntaxKind::Function => {
                            modifiers |= HighlightModifier::Declaration;
                            HighlightKind::Function
                        }
                        _ => continue,
                    },

                    _ => continue,
                };

                hi as u32
            };

            let range = token.range(self.parse.syntax_tree());

            let (line, column) = self.line_index.line_col(range.start());

            let (delta_line, delta_column) = match prev_token_position {
                Some((prev_line, prev_column)) if prev_line == line => {
                    (LineNr(0), column - prev_column)
                }
                Some((prev_line, _)) => (line - prev_line, column),
                None => (line, column),
            };

            prev_token_position = Some((line, column));

            tokens.push(SemanticToken {
                delta_line: delta_line.0,
                delta_start: delta_column.0,
                length: u32::from(range.len()),
                token_type,
                token_modifiers_bitset: modifiers.0,
            });
        }

        tokens
    }

    fn diagnostics(&self) -> Vec<Diagnostic> {
        let syntax_errors =
            self.parse.errors().iter().copied().map(diagnostics::Diagnostic::from_syntax);

        let validation_diagnostics = self
            .validation_diagnostics
            .iter()
            .copied()
            .map(diagnostics::Diagnostic::from_validation);

        let indexing_diagnostics =
            self.indexing_diagnostics.iter().cloned().map(diagnostics::Diagnostic::from_indexing);

        let lowering_diagnostics =
            self.lowering_diagnostics.iter().cloned().map(diagnostics::Diagnostic::from_lowering);

        let ty_diagnostics =
            self.ty_diagnostics.iter().cloned().map(diagnostics::Diagnostic::from_ty);

        let diagnostics = syntax_errors
            .chain(validation_diagnostics)
            .chain(indexing_diagnostics)
            .chain(lowering_diagnostics)
            .chain(ty_diagnostics);

        diagnostics
            .map(|diagnostic| Diagnostic {
                range: convert_text_range(diagnostic.range(), &self.line_index),
                severity: Some(convert_diagnostic_severity(diagnostic.severity())),
                code: None,
                code_description: None,
                source: Some("gb".to_string()),
                message: diagnostic.message(),
                related_information: None,
                tags: None,
                data: None,
            })
            .collect()
    }

    fn update_line_index(&mut self) {
        self.line_index = LineIndex::new(&self.content);
    }

    fn reparse(&mut self) {
        let tokens = lexer::lex(&self.content);
        self.parse = parser::parse_source_file(&tokens, &self.content);
        let tree = self.parse.syntax_tree();
        self.ast = ast::Root::cast(tree.root(), tree).unwrap();
    }

    fn validate(&mut self) {
        self.validation_diagnostics = ast::validation::validate(self.ast, self.parse.syntax_tree());
    }

    fn index(&mut self, world_index: &hir::WorldIndex) {
        let (index, diagnostics) = hir::index(self.ast, self.parse.syntax_tree(), world_index);
        self.index = index;
        self.indexing_diagnostics = diagnostics;
    }

    fn lower(&mut self, world_index: &hir::WorldIndex) {
        let (bodies, diagnostics) =
            hir::lower(self.ast, self.parse.syntax_tree(), &self.index, world_index);
        self.bodies = bodies;
        self.lowering_diagnostics = diagnostics;
    }

    fn infer(&mut self, world_index: &hir::WorldIndex) {
        let (results, diagnostics) = hir_ty::infer_all(&self.bodies, &self.index, world_index);
        self.inference_result = results;
        self.ty_diagnostics = diagnostics;
    }
}

#[derive(Clone, Copy)]
pub enum HighlightKind {
    Keyword,
    Local,
    Param,
    Function,
    Number,
    String,
    Operator,
    Comment,
    __Last,
}

impl HighlightKind {
    pub fn all_lsp() -> Vec<SemanticTokenType> {
        Self::all().into_iter().map(Self::to_lsp).collect()
    }

    fn all() -> [Self; Self::__Last as usize] {
        let mut values = [0; Self::__Last as usize];

        for i in 0..Self::__Last as u8 {
            values[i as usize] = i;
        }

        unsafe { mem::transmute(values) }
    }

    fn to_lsp(self) -> SemanticTokenType {
        match self {
            Self::Keyword => SemanticTokenType::KEYWORD,
            Self::Local => SemanticTokenType::VARIABLE,
            Self::Param => SemanticTokenType::PARAMETER,
            Self::Function => SemanticTokenType::FUNCTION,
            Self::Number => SemanticTokenType::NUMBER,
            Self::String => SemanticTokenType::STRING,
            Self::Operator => SemanticTokenType::OPERATOR,
            Self::Comment => SemanticTokenType::COMMENT,
            Self::__Last => unreachable!(),
        }
    }
}

#[derive(Clone, Copy)]
pub enum HighlightModifier {
    Declaration,
    __Last,
}

impl HighlightModifier {
    pub fn all_lsp() -> Vec<SemanticTokenModifier> {
        Self::all().into_iter().map(Self::to_lsp).collect()
    }

    fn all() -> [Self; Self::__Last as usize] {
        let mut values = [0; Self::__Last as usize];

        for i in 0..Self::__Last as u8 {
            values[i as usize] = i;
        }

        unsafe { mem::transmute(values) }
    }

    fn to_lsp(self) -> SemanticTokenModifier {
        match self {
            Self::Declaration => SemanticTokenModifier::DECLARATION,
            Self::__Last => unreachable!(),
        }
    }

    fn mask(self) -> u32 {
        1 << self as u32
    }
}

pub struct HighlightModifiers(u32);

impl BitOrAssign<HighlightModifier> for HighlightModifiers {
    fn bitor_assign(&mut self, rhs: HighlightModifier) {
        self.0 |= rhs.mask();
    }
}

fn convert_lsp_range(range: Range, line_index: &LineIndex) -> TextRange {
    TextRange::new(
        convert_lsp_position(range.start, line_index),
        convert_lsp_position(range.end, line_index),
    )
}

fn convert_lsp_position(position: Position, line_index: &LineIndex) -> TextSize {
    line_index[LineNr(position.line)] + TextSize::from(position.character)
}

fn convert_text_range(range: TextRange, line_index: &LineIndex) -> Range {
    Range {
        start: convert_text_size(range.start(), line_index),
        end: convert_text_size(range.end(), line_index),
    }
}

fn convert_text_size(offset: TextSize, line_index: &LineIndex) -> Position {
    let (LineNr(line), ColNr(character)) = line_index.line_col(offset);
    Position { line, character }
}

fn convert_diagnostic_severity(severity: diagnostics::Severity) -> DiagnosticSeverity {
    match severity {
        diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
        diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
    }
}
