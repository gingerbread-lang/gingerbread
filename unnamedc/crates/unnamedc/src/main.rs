use ast::AstNode;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{ContentStyle, StyledContent, Stylize};
use crossterm::{cursor, queue, terminal};
use errors::Error;
use eval::Evaluator;
use std::convert::TryInto;
use std::io::{self, Write};
use token::TokenKind;

fn main() -> anyhow::Result<()> {
    terminal::enable_raw_mode()?;

    let mut stdout = io::stdout();
    let mut input = String::new();
    let mut cursor_pos: u16 = 0;
    let mut evaluator = Evaluator::default();
    let mut hir_in_scope = hir_lower::InScope::default();
    let mut tys_in_scope = hir_ty::InScope::default();

    queue!(stdout, cursor::SetCursorShape(cursor::CursorShape::Line))?;
    write!(stdout, "> ")?;
    stdout.flush()?;

    loop {
        if let Event::Key(key_event) = event::read()? {
            let mut pressed_enter = false;

            match key_event {
                KeyEvent { code: KeyCode::Char('d'), modifiers: KeyModifiers::CONTROL } => break,
                KeyEvent { code: KeyCode::Char('l'), modifiers: KeyModifiers::CONTROL } => {
                    queue!(
                        stdout,
                        terminal::Clear(terminal::ClearType::All),
                        cursor::MoveTo(0, 0)
                    )?;
                }
                KeyEvent { code: KeyCode::Char('a'), modifiers: KeyModifiers::CONTROL } => {
                    cursor_pos = 0;
                }
                KeyEvent { code: KeyCode::Char('e'), modifiers: KeyModifiers::CONTROL } => {
                    cursor_pos = input.len() as u16;
                }
                KeyEvent { code: KeyCode::Char(c), modifiers: KeyModifiers::NONE } => {
                    input.insert(cursor_pos.into(), c);
                    cursor_pos += 1;
                }
                KeyEvent { code: KeyCode::Backspace, modifiers: KeyModifiers::NONE } => {
                    if cursor_pos != 0 {
                        cursor_pos -= 1;
                        input.remove(cursor_pos.into());
                    }
                }
                KeyEvent { code: KeyCode::Enter, modifiers: KeyModifiers::NONE } => {
                    pressed_enter = true;
                }
                KeyEvent { code: KeyCode::Left, modifiers: KeyModifiers::NONE } => {
                    if cursor_pos != 0 {
                        cursor_pos -= 1;
                    }
                }
                KeyEvent { code: KeyCode::Right, modifiers: KeyModifiers::NONE } => {
                    if usize::from(cursor_pos) < input.len() {
                        cursor_pos += 1;
                    }
                }
                _ => {}
            }

            render(
                &mut input,
                &mut stdout.lock(),
                &mut hir_in_scope,
                &mut tys_in_scope,
                pressed_enter,
                &mut evaluator,
                &mut cursor_pos,
            )?;
        }
    }

    queue!(stdout, cursor::SetCursorShape(cursor::CursorShape::Block))?;
    stdout.flush()?;

    terminal::disable_raw_mode()?;

    Ok(())
}

fn render(
    input: &mut String,
    stdout: &mut io::StdoutLock<'_>,
    hir_in_scope: &mut hir_lower::InScope,
    tys_in_scope: &mut hir_ty::InScope,
    pressed_enter: bool,
    evaluator: &mut Evaluator,
    cursor_pos: &mut u16,
) -> anyhow::Result<()> {
    let mut errors = Vec::new();

    let tokens = lexer::lex(input);
    let parse = parser::parse_repl_line(&tokens);

    for error in parse.errors() {
        errors.push(Error::from_parse_error(error.clone()));
    }

    let root = ast::Root::cast(parse.syntax_node()).unwrap();
    let validation_errors = ast::validation::validate(&root);

    for error in validation_errors {
        errors.push(Error::from_validation_error(error));
    }

    let lower_result = hir_lower::lower_with_in_scope(&root, hir_in_scope.clone());

    for error in lower_result.errors {
        errors.push(Error::from_lower_error(error));
    }

    let infer_result = hir_ty::infer_in_scope(&lower_result.program, tys_in_scope.clone());
    let (tys_in_scope_new, ty_errors) = infer_result.in_scope();

    for error in ty_errors {
        errors.push(Error::from_ty_error(error, &lower_result.source_map));
    }

    queue!(stdout, terminal::Clear(terminal::ClearType::CurrentLine), cursor::MoveToColumn(0))?;

    write!(stdout, "> ")?;
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

        write!(stdout, "{}", c)?;
    }

    if pressed_enter {
        writeln!(stdout, "\r")?;

        if errors.is_empty() {
            *hir_in_scope = hir_lower::InScope::new(
                lower_result.program.clone(),
                lower_result.fnc_names,
                lower_result.var_names,
            );
            *tys_in_scope = tys_in_scope_new;
            let result = evaluator.eval(lower_result.program);

            writeln!(stdout, "{:?}\r", result)?;

            input.clear();
            *cursor_pos = 0;
        } else {
            for error in errors {
                for line in error.display(input) {
                    writeln!(stdout, "{}\r", line)?;
                }
            }
        }

        write!(stdout, "> ")?;

        render(input, stdout, hir_in_scope, tys_in_scope, false, evaluator, cursor_pos)?;
    }

    queue!(stdout, cursor::MoveToColumn(3 + *cursor_pos))?;

    stdout.flush()?;

    Ok(())
}
