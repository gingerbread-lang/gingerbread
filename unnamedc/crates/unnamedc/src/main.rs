use ast::AstNode;
use crossterm::event::{self, Event, KeyCode, KeyEvent, KeyModifiers};
use crossterm::style::{ContentStyle, StyledContent, Stylize};
use crossterm::{cursor, queue, terminal};
use errors::Error;
use eval::Evaluator;
use hir_ty::Ty;
use la_arena::{Arena, ArenaMap};
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
    let mut var_defs = Arena::new();
    let mut var_def_names = HashMap::new();
    let mut var_tys = ArenaMap::default();

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
                &mut var_defs,
                &mut var_def_names,
                &mut var_tys,
                pressed_enter,
                &mut evaluator,
                &mut cursor_pos,
            )?;
        }
    }

    terminal::disable_raw_mode()?;

    Ok(())
}

fn render(
    input: &mut String,
    stdout: &mut io::StdoutLock<'_>,
    var_defs: &mut Arena<hir::VarDef>,
    var_def_names: &mut HashMap<String, hir::VarDefIdx>,
    var_tys: &mut ArenaMap<hir::VarDefIdx, Ty>,
    pressed_enter: bool,
    evaluator: &mut Evaluator,
    cursor_pos: &mut u16,
) -> anyhow::Result<()> {
    let mut errors = Vec::new();

    let tokens = lexer::lex(input);
    let parse = parser::parse(&tokens);

    for error in parse.errors() {
        errors.push(Error::from_parse_error(error.clone()));
    }

    let root = ast::Root::cast(parse.syntax_node()).unwrap();
    let validation_errors = ast::validation::validate(&root);

    for error in validation_errors {
        errors.push(Error::from_validation_error(error));
    }

    let (program, source_map, lower_errors, new_var_def_names) =
        hir_lower::lower_with_var_defs(&root, var_defs.clone(), var_def_names.clone());

    for error in lower_errors {
        errors.push(Error::from_lower_error(error));
    }

    let infer_result = hir_ty::infer_with_var_tys(&program, var_tys.clone());

    for error in infer_result.errors {
        errors.push(Error::from_ty_error(error, &source_map));
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
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::LBrace
            | TokenKind::RBrace => c.dark_grey(),
            TokenKind::Whitespace => StyledContent::new(ContentStyle::new(), c),
            TokenKind::Error => c.red().bold(),
        };

        let c = if is_in_error { c.underlined() } else { c };

        write!(stdout, "{}", c)?;
    }

    if pressed_enter {
        writeln!(stdout, "\r")?;

        if errors.is_empty() {
            *var_defs = program.var_defs.clone();
            *var_def_names = new_var_def_names;
            *var_tys = infer_result.var_tys;
            let result = evaluator.eval(program);

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

        render(input, stdout, var_defs, var_def_names, var_tys, false, evaluator, cursor_pos)?;
    }

    queue!(stdout, cursor::MoveToColumn(3 + *cursor_pos))?;

    stdout.flush()?;

    Ok(())
}
