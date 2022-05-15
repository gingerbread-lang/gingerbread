mod body;
mod index;
mod index_diff;
mod project;
mod world_index;

pub use self::body::*;
pub use self::index::*;
pub use self::index_diff::*;
pub use self::project::*;
pub use self::world_index::*;

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
    Unit,
}
