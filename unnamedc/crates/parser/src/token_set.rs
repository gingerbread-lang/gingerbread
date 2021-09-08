use std::ops::BitOr;
use token::TokenKind;

// Each bit represents whether that bit’s TokenKind is in the set.
//
// This is a TokenSet containing the first and third variants of TokenKind
// (regardless of what they may be):
//
//     0000000000000101
//
// Thus, the number of TokenKind variants must not exceed
// the number of bits in TokenSet.
//
// This implementation is mostly stolen from rust-analyzer:
// https://github.com/rust-analyzer/rust-analyzer/blob/b73b321478d3b2a98d380eb79de717e01620c4e9/crates/parser/src/token_set.rs
#[derive(Debug, Clone, Copy)]
pub(crate) struct TokenSet(u16);

impl TokenSet {
    pub(crate) const fn new<const LEN: usize>(kinds: [TokenKind; LEN]) -> Self {
        let mut value = 0;

        let mut idx = 0;
        while idx < kinds.len() {
            value |= mask(kinds[idx]);
            idx += 1;
        }

        Self(value)
    }

    pub(crate) fn contains(self, kind: TokenKind) -> bool {
        self.0 & mask(kind) != 0
    }
}

impl BitOr for TokenSet {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

const fn mask(kind: TokenKind) -> u16 {
    1 << kind as u16
}

#[cfg(test)]
#[test]
fn it_works() {
    assert_eq!(TokenKind::LetKw as u16, 0);
    assert_eq!(TokenKind::Int as u16, 2);
    assert_eq!(TokenKind::String as u16, 3);

    let set = TokenSet::new([TokenKind::LetKw, TokenKind::Int]);
    assert_eq!(set.0, 0b0000000000000101);
    //             pos:              2 0

    assert!(set.contains(TokenKind::LetKw));
    assert!(set.contains(TokenKind::Int));
    assert!(!set.contains(TokenKind::String));

    let set = set | TokenSet::new([TokenKind::String]);
    assert_eq!(set.0, 0b0000000000001101);
    //             pos:             32 0

    assert!(set.contains(TokenKind::LetKw));
    assert!(set.contains(TokenKind::Int));
    assert!(set.contains(TokenKind::String));
    assert!(!set.contains(TokenKind::Ident));
}
