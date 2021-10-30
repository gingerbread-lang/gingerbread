mod expr;
mod stmt;
mod ty;

use crate::parser::Parser;
use syntax::SyntaxKind;

pub(crate) fn source_file(p: &mut Parser<'_, '_>) {
    let m = p.start();

    loop {
        if p.at_eof() {
            break;
        }

        self::stmt::parse_stmt(p);
    }

    m.complete(p, SyntaxKind::SourceFile);
}
