use ast::AstNode;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{ContentStyle, StyledContent, Stylize};
use crossterm::{cursor, queue, terminal};
use errors::Error;
use eval::Evaluator;
use std::convert::TryInto;
use std::io::{self, Write};
use std::mem;
use token::{Token, TokenKind};

fn main() -> anyhow::Result<()> {
    terminal::enable_raw_mode()?;

    let mut stdout = io::stdout();
    let mut editor = Editor::default();
    let mut evaluator = Evaluator::default();
    let mut analysis_state = AnalysisState::default();

    queue!(stdout, cursor::SetCursorShape(cursor::CursorShape::Line))?;
    write!(stdout, "> ")?;
    stdout.flush()?;

    loop {
        if let Event::Key(key_event) = event::read()? {
            let mut pressed_enter = false;

            match editor.handle_key_event(key_event) {
                KeyPressHandleResult::Editing => {}
                KeyPressHandleResult::SubmitLine => pressed_enter = true,
                KeyPressHandleResult::Exit => break,
                KeyPressHandleResult::Clear => {
                    queue!(stdout, terminal::Clear(terminal::ClearType::All), cursor::MoveTo(0, 0))?
                }
            }

            let (new_analysis_state, should_redraw, should_clear) = render(
                &editor.buf,
                editor.cursor_pos,
                &mut stdout.lock(),
                analysis_state,
                pressed_enter,
                &mut evaluator,
            )?;

            analysis_state = new_analysis_state;

            if should_clear {
                editor.clear();
            }

            if should_redraw {
                let (new_analysis_state, ..) = render(
                    &editor.buf,
                    editor.cursor_pos,
                    &mut stdout.lock(),
                    analysis_state,
                    false,
                    &mut evaluator,
                )?;

                analysis_state = new_analysis_state;
            }
        }
    }

    queue!(stdout, cursor::SetCursorShape(cursor::CursorShape::Block))?;
    stdout.flush()?;

    terminal::disable_raw_mode()?;

    Ok(())
}

#[derive(Default)]
struct Editor {
    cursor_pos: u16,
    buf: String,
    clipboard: String,
}

impl Editor {
    fn handle_key_event(&mut self, key_event: KeyEvent) -> KeyPressHandleResult {
        match key_event {
            KeyEvent { code: KeyCode::Char('d'), modifiers: KeyModifiers::CONTROL } => {
                return KeyPressHandleResult::Exit;
            }

            KeyEvent { code: KeyCode::Char('l'), modifiers: KeyModifiers::CONTROL } => {
                return KeyPressHandleResult::Clear;
            }

            KeyEvent { code: KeyCode::Char('a'), modifiers: KeyModifiers::CONTROL } => {
                self.cursor_pos = 0;
            }

            KeyEvent { code: KeyCode::Char('e'), modifiers: KeyModifiers::CONTROL } => {
                self.cursor_pos = self.buf.len() as u16;
            }

            KeyEvent { code: KeyCode::Char('u'), modifiers: KeyModifiers::CONTROL } => {
                self.clipboard = self.buf.split_off(self.cursor_pos as usize);
                mem::swap(&mut self.clipboard, &mut self.buf);
                self.cursor_pos = 0;
            }

            KeyEvent { code: KeyCode::Char('k'), modifiers: KeyModifiers::CONTROL } => {
                self.clipboard = self.buf.split_off(self.cursor_pos as usize);
            }

            KeyEvent { code: KeyCode::Char('y'), modifiers: KeyModifiers::CONTROL } => {
                self.buf.insert_str(self.cursor_pos as usize, &self.clipboard);
                self.cursor_pos += self.clipboard.len() as u16;
            }

            KeyEvent { code: KeyCode::Char(c), modifiers: KeyModifiers::NONE } => {
                self.buf.insert(self.cursor_pos.into(), c);
                self.cursor_pos += 1;
            }

            KeyEvent { code: KeyCode::Backspace, modifiers: KeyModifiers::NONE } => {
                if self.cursor_pos != 0 {
                    self.cursor_pos -= 1;
                    self.buf.remove(self.cursor_pos.into());
                }
            }

            KeyEvent { code: KeyCode::Enter, modifiers: KeyModifiers::NONE } => {
                return KeyPressHandleResult::SubmitLine;
            }

            KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE } => {
                if self.cursor_pos != 0 {
                    self.cursor_pos -= 1;
                }
            }

            KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE } => {
                if usize::from(self.cursor_pos) < self.buf.len() {
                    self.cursor_pos += 1;
                }
            }

            _ => {}
        }

        KeyPressHandleResult::Editing
    }

    fn clear(&mut self) {
        self.buf.clear();
        self.cursor_pos = 0;
    }
}

enum KeyPressHandleResult {
    Editing,
    SubmitLine,
    Exit,
    Clear,
}

fn render(
    input: &str,
    cursor_pos: u16,
    stdout: &mut io::StdoutLock<'_>,
    analysis_state: AnalysisState,
    pressed_enter: bool,
    evaluator: &mut Evaluator,
) -> anyhow::Result<(AnalysisState, bool, bool)> {
    let mut should_redraw = false;
    let mut should_clear = false;

    queue!(stdout, terminal::Clear(terminal::ClearType::CurrentLine), cursor::MoveToColumn(0))?;
    write!(stdout, "> ")?;

    let analysis = analysis_state.clone().analyze_repl_line(input);

    write!(stdout, "{}", analysis.highlighted)?;

    let analysis_state = match analysis.new_state {
        Ok((new_state, program)) if pressed_enter => {
            let result = evaluator.eval(program);
            writeln!(stdout, "\r")?;
            writeln!(stdout, "{:?}\r", result)?;

            should_redraw = true;
            should_clear = true;

            new_state
        }

        Ok(_) => analysis_state,

        Err((old_state, errors)) if pressed_enter => {
            writeln!(stdout, "\r")?;

            for error in errors {
                for line in error.display(input) {
                    writeln!(stdout, "{}\r", line)?;
                }
            }

            should_redraw = true;

            old_state
        }

        Err((old_state, _)) => old_state,
    };

    queue!(stdout, cursor::MoveToColumn(3 + cursor_pos))?;

    stdout.flush()?;

    Ok((analysis_state, should_redraw, should_clear))
}

#[derive(Clone, Default)]
struct AnalysisState {
    hir_in_scope: hir_lower::InScope,
    tys_in_scope: hir_ty::InScope,
}

impl AnalysisState {
    fn analyze_repl_line(self, input: &str) -> Analysis {
        let mut errors = Vec::new();

        let tokens = lexer::lex(input);
        let parse = parser::parse_repl_line(&tokens);

        for error in parse.errors() {
            errors.push(Error::from_syntax_error(error.clone()));
        }

        let root = ast::Root::cast(parse.syntax_node()).unwrap();
        let validation_errors = ast::validation::validate(&root);

        for error in validation_errors {
            errors.push(Error::from_validation_error(error));
        }

        let lower_result = hir_lower::lower_with_in_scope(&root, self.hir_in_scope.clone());

        for error in lower_result.errors {
            errors.push(Error::from_lower_error(error));
        }

        let infer_result = hir_ty::infer_in_scope(&lower_result.program, self.tys_in_scope.clone());
        let (tys_in_scope_new, ty_errors) = infer_result.in_scope();

        for error in ty_errors {
            errors.push(Error::from_ty_error(error, &lower_result.source_map));
        }

        let highlighted = highlight(input, &errors, tokens);

        let new_state = if errors.is_empty() {
            Ok((
                Self {
                    hir_in_scope: hir_lower::InScope::new(
                        lower_result.program.clone(),
                        lower_result.fnc_names,
                        lower_result.var_names,
                    ),
                    tys_in_scope: tys_in_scope_new,
                },
                lower_result.program,
            ))
        } else {
            Err((self, errors))
        };

        Analysis { highlighted, new_state }
    }
}

fn highlight(input: &str, errors: &[Error], tokens: Vec<Token<'_>>) -> String {
    let mut highlighted = String::new();

    for (idx, c) in input.char_indices() {
        let idx = idx.try_into().unwrap();
        let is_in_error = errors.iter().any(|error| error.range().contains(idx));

        let token = tokens.iter().find(|token| token.range.contains(idx)).unwrap();
        let c = match token.kind {
            TokenKind::LetKw | TokenKind::FncKw => c.dark_magenta(),
            TokenKind::Ident => c.dark_blue(),
            TokenKind::Int => c.dark_yellow(),
            TokenKind::String => c.dark_green(),
            TokenKind::Plus | TokenKind::Hyphen | TokenKind::Asterisk | TokenKind::Slash => {
                c.dark_cyan()
            }
            TokenKind::Eq
            | TokenKind::Colon
            | TokenKind::Comma
            | TokenKind::Semicolon
            | TokenKind::Arrow
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::LBrace
            | TokenKind::RBrace => c.dark_grey(),
            TokenKind::Whitespace => StyledContent::new(ContentStyle::new(), c),
            TokenKind::Comment => c.dark_grey().italic(),
            TokenKind::Error => c.red().bold(),
        };

        let c = if is_in_error { c.underlined() } else { c };
        highlighted.push_str(&c.to_string());
    }

    highlighted
}

struct Analysis {
    highlighted: String,
    new_state: Result<(AnalysisState, hir::Program), (AnalysisState, Vec<Error>)>,
}
