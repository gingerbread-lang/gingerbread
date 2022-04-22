use ast::validation::ValidationDiagnostic;
use ast::{AstNode, AstToken};
use diagnostics::Diagnostic;
use interner::Interner;
use line_index::LineIndex;
use parser::Parse;
use std::collections::HashMap;
use std::mem;
use std::ops::BitOrAssign;
use syntax::{NodeKind, SyntaxElement, TokenKind};
use text_size::{TextRange, TextSize};
use url::Url;

#[derive(Default)]
pub struct GlobalState {
    interner: Interner,
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

        let analysis = Analysis::new(
            content,
            hir::Name(self.interner.intern(module_name)),
            &mut self.interner,
            &mut self.world_index,
        );

        for analysis in self.analyses.values_mut() {
            analysis.recheck(&mut self.world_index, &mut self.interner);
        }

        self.analyses.insert(uri, analysis);
    }

    pub fn update_contents(&mut self, uri: &Url, f: impl FnOnce(&mut String, &LineIndex)) {
        self.analyses.get_mut(uri).unwrap().update_contents(
            f,
            &mut self.interner,
            &mut self.world_index,
        );

        for (analysis_uri, analysis) in &mut self.analyses {
            if analysis_uri == uri {
                continue;
            }

            analysis.recheck(&mut self.world_index, &mut self.interner);
        }
    }

    pub fn parent_ranges(&self, uri: &Url, offset: TextSize) -> Vec<TextRange> {
        self.analyses[uri].parent_ranges(offset)
    }

    pub fn highlight(&self, uri: &Url) -> Vec<Highlight> {
        self.analyses[uri].highlight()
    }

    pub fn diagnostics(&self) -> impl Iterator<Item = (&Url, Vec<Diagnostic>)> {
        self.analyses.iter().map(|(uri, analysis)| (uri, analysis.diagnostics()))
    }

    pub fn line_index(&self, uri: &Url) -> &LineIndex {
        &self.analyses[uri].line_index
    }

    pub fn interner(&self) -> &Interner {
        &self.interner
    }
}

impl Analysis {
    fn new(
        content: String,
        module_name: hir::Name,
        interner: &mut Interner,
        world_index: &mut hir::WorldIndex,
    ) -> Self {
        let parse = {
            let tokens = lexer::lex(&content);
            parser::parse_source_file(&tokens, &content)
        };
        let tree = parse.syntax_tree();
        let ast = ast::Root::cast(tree.root(), tree).unwrap();
        let (index, indexing_diagnostics) = hir::index(ast, tree, world_index, interner);
        let (bodies, lowering_diagnostics) = hir::lower(ast, tree, &index, world_index, interner);
        let (inference_result, ty_diagnostics) = hir_ty::infer_all(&bodies, &index, world_index);

        world_index.add_module(module_name, index.clone());

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
            inference_result,
            ty_diagnostics,
        };

        analysis.update_line_index();
        analysis.validate();

        analysis
    }

    fn update_contents(
        &mut self,
        f: impl FnOnce(&mut String, &LineIndex),
        interner: &mut Interner,
        world_index: &mut hir::WorldIndex,
    ) {
        f(&mut self.content, &self.line_index);
        self.update_line_index();
        self.reparse();
        self.validate();
        self.index(world_index, interner);
        self.recheck(world_index, interner);
    }

    fn recheck(&mut self, world_index: &mut hir::WorldIndex, interner: &mut Interner) {
        self.lower(world_index, interner);
        world_index.update_module(self.module_name, self.index.clone());

        self.infer(world_index);
    }

    fn parent_ranges(&self, offset: TextSize) -> Vec<TextRange> {
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

            if token.kind(self.parse.syntax_tree()) == TokenKind::Whitespace {
                continue;
            }

            if range.contains_inclusive(offset) {
                push_range(range);
                break;
            }
        }

        ranges
    }

    fn highlight(&self) -> Vec<Highlight> {
        let mut tokens = Vec::new();
        let mut last_parent_node_kind = NodeKind::Root;

        for element in self.ast.syntax().descendants(self.parse.syntax_tree()) {
            let token = match element {
                SyntaxElement::Node(node) => {
                    last_parent_node_kind = node.kind(self.parse.syntax_tree());
                    continue;
                }
                SyntaxElement::Token(token) => token,
            };

            let mut modifiers = HighlightModifiers(0);

            let kind = match token.kind(self.parse.syntax_tree()) {
                TokenKind::LetKw | TokenKind::FncKw => HighlightKind::Keyword,
                TokenKind::Int => HighlightKind::Number,
                TokenKind::String => HighlightKind::String,
                TokenKind::Plus | TokenKind::Hyphen | TokenKind::Asterisk | TokenKind::Slash => {
                    HighlightKind::Operator
                }
                TokenKind::Comment => HighlightKind::Comment,

                TokenKind::Ident => match last_parent_node_kind {
                    NodeKind::LocalDef => {
                        modifiers |= HighlightModifier::Declaration;
                        HighlightKind::Local
                    }
                    NodeKind::Param => {
                        modifiers |= HighlightModifier::Declaration;
                        HighlightKind::Param
                    }
                    NodeKind::Function => {
                        modifiers |= HighlightModifier::Declaration;
                        HighlightKind::Function
                    }
                    _ => {
                        let ident = ast::Ident::cast(token, self.parse.syntax_tree()).unwrap();
                        match self.bodies.symbol(ident) {
                            Some(hir::Symbol::Local) => HighlightKind::Local,
                            Some(hir::Symbol::Param) => HighlightKind::Param,
                            Some(hir::Symbol::Function) => HighlightKind::Function,
                            Some(hir::Symbol::Module) => HighlightKind::Module,
                            None if self.index.is_ident_ty(ident) => HighlightKind::Ty,
                            None => continue,
                        }
                    }
                },

                _ => continue,
            };

            let range = token.range(self.parse.syntax_tree());

            tokens.push(Highlight { range, kind, modifiers });
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

        syntax_errors
            .chain(validation_diagnostics)
            .chain(indexing_diagnostics)
            .chain(lowering_diagnostics)
            .chain(ty_diagnostics)
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

    fn index(&mut self, world_index: &hir::WorldIndex, interner: &mut Interner) {
        let (index, diagnostics) =
            hir::index(self.ast, self.parse.syntax_tree(), world_index, interner);
        self.index = index;
        self.indexing_diagnostics = diagnostics;
    }

    fn lower(&mut self, world_index: &hir::WorldIndex, interner: &mut Interner) {
        let (bodies, diagnostics) =
            hir::lower(self.ast, self.parse.syntax_tree(), &self.index, world_index, interner);
        self.bodies = bodies;
        self.lowering_diagnostics = diagnostics;
    }

    fn infer(&mut self, world_index: &hir::WorldIndex) {
        let (results, diagnostics) = hir_ty::infer_all(&self.bodies, &self.index, world_index);
        self.inference_result = results;
        self.ty_diagnostics = diagnostics;
    }
}

pub struct Highlight {
    pub range: TextRange,
    pub kind: HighlightKind,
    pub modifiers: HighlightModifiers,
}

#[derive(Clone, Copy)]
pub enum HighlightKind {
    Keyword,
    Local,
    Param,
    Function,
    Module,
    Ty,
    Number,
    String,
    Operator,
    Comment,
    __Last,
}

impl HighlightKind {
    pub fn all() -> [Self; Self::__Last as usize] {
        let mut values = [0; Self::__Last as usize];

        for i in 0..Self::__Last as u8 {
            values[i as usize] = i;
        }

        unsafe { mem::transmute(values) }
    }
}

#[derive(Clone, Copy)]
pub enum HighlightModifier {
    Declaration,
    __Last,
}

impl HighlightModifier {
    pub fn all() -> [Self; Self::__Last as usize] {
        let mut values = [0; Self::__Last as usize];

        for i in 0..Self::__Last as u8 {
            values[i as usize] = i;
        }

        unsafe { mem::transmute(values) }
    }

    fn mask(self) -> u32 {
        1 << self as u32
    }
}

pub struct HighlightModifiers(u32);

impl HighlightModifiers {
    pub fn into_raw(self) -> u32 {
        self.0
    }
}

impl BitOrAssign<HighlightModifier> for HighlightModifiers {
    fn bitor_assign(&mut self, rhs: HighlightModifier) {
        self.0 |= rhs.mask();
    }
}
