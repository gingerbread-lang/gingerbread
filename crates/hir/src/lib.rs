mod body;
mod index;
mod nameres;
mod project;
mod world_index;

pub use self::body::*;
pub use self::index::*;
pub use self::nameres::*;
pub use self::project::*;
pub use self::world_index::*;

use interner::Interner;
use interner::Key;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Name(pub Key);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Fqn {
    pub module: Name,
    pub name: Name,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Ty {
    Unknown,
    S32,
    String,
    Named(Name),
    Unit,
}

impl Ty {
    pub fn display(self, interner: &Interner) -> &str {
        match self {
            Self::Unknown => "?",
            Self::S32 => "s32",
            Self::String => "string",
            Self::Named(n) => interner.lookup(n.0),
            Self::Unit => "unit",
        }
    }
}
