use ast::AstNode;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{ContentStyle, StyledContent, Stylize};
use crossterm::{cursor, queue, terminal};
use eval::Evaluator;
use std::collections::HashMap;
use std::convert::TryInto;
use std::io::{self, Write};
use token::TokenKind;

fn main() -> anyhow::Result<()> {
    terminal::enable_raw_mode()?;

    let mut stdout = io::stdout();
    let mut input = String::new();
    let mut cursor_pos: u16 = 0;
    let mut evaluator = Evaluator::default();
    let mut var_tys = HashMap::new();

    write!(stdout, "> ")?;
    stdout.flush()?;

    loop {
        if let Event::Key(key_event) = event::read()? {
            let mut pressed_enter = false;

            match key_event {
                KeyEvent { code: KeyCode::Char('d'), modifiers: KeyModifiers::CONTROL } => break,
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
                    cursor_pos = 0;
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

            let mut error_ranges = Vec::new();

            let tokens = lexer::lex(&input);
            let parse = parser::parse(&tokens);

            for error in parse.errors() {
                error_ranges.push(error.range);
            }

            let root = ast::Root::cast(parse.syntax_node()).unwrap();
            let validation_errors = ast::validation::validate(&root);

            for error in validation_errors {
                error_ranges.push(error.range);
            }

            let (program, source_map) = hir_lower::lower(&root);

            let infer_result = hir_ty::infer_with_var_tys(&program, var_tys.clone());

            for error in infer_result.errors {
                let ast = &source_map.expr_map[error.expr];
                error_ranges.push(ast.range());
            }

            queue!(
                stdout,
                terminal::Clear(terminal::ClearType::CurrentLine),
                cursor::MoveToColumn(0)
            )?;

            write!(stdout, "> ")?;
            for (idx, c) in input.char_indices() {
                let idx = idx.try_into().unwrap();
                let is_in_error = error_ranges.iter().any(|range| range.contains(idx));

                let token = tokens.iter().find(|token| token.range.contains(idx)).unwrap();
                let c = match token.kind {
                    TokenKind::LetKw => c.magenta().bold(),
                    TokenKind::Ident => c.blue(),
                    TokenKind::Int => c.yellow(),
                    TokenKind::String => c.green(),
                    TokenKind::Plus
                    | TokenKind::Hyphen
                    | TokenKind::Asterisk
                    | TokenKind::Slash => c.cyan(),
                    TokenKind::Eq | TokenKind::LParen | TokenKind::RParen => c.dark_grey(),
                    TokenKind::Whitespace => StyledContent::new(ContentStyle::new(), c),
                    TokenKind::Error => c.red().bold(),
                };

                let c = if is_in_error { c.underlined() } else { c };

                write!(stdout, "{}", c)?;
            }

            if pressed_enter && error_ranges.is_empty() {
                var_tys = infer_result.var_tys;
                let result = evaluator.eval(program);

                queue!(stdout, cursor::MoveToNextLine(0))?;
                write!(stdout, "{:?}\r\n> ", result)?;

                input.clear();
            }

            queue!(stdout, cursor::MoveToColumn(3 + cursor_pos))?;

            stdout.flush()?;
        }
    }

    terminal::disable_raw_mode()?;

    Ok(())
}
