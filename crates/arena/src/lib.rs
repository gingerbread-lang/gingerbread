mod id;
mod id_range;
mod map;

pub use self::id::Id;
pub use self::id_range::builder::IdRangeBuilder;
pub use self::id_range::IdRange;
pub use self::map::ArenaMap;

use std::fmt;
use std::iter::FromIterator;
use std::ops::{Index, IndexMut};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<T> Arena<T> {
    pub const fn new() -> Arena<T> {
        Arena { data: Vec::new() }
    }

    pub fn clear(&mut self) {
        self.data.clear();
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }

    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn alloc(&mut self, value: T) -> Id<T> {
        let id = Id::from_raw(self.data.len() as u32);
        self.data.push(value);
        id
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (Id<T>, &T)> + ExactSizeIterator + DoubleEndedIterator {
        self.data.iter().enumerate().map(|(raw, value)| (Id::from_raw(raw as u32), value))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (Id<T>, &mut T)> + ExactSizeIterator + DoubleEndedIterator {
        self.data.iter_mut().enumerate().map(|(raw, value)| (Id::from_raw(raw as u32), value))
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena { data: Vec::new() }
    }
}

impl<T> Index<Id<T>> for Arena<T> {
    type Output = T;
    fn index(&self, id: Id<T>) -> &T {
        &self.data[(id.raw as usize)]
    }
}

impl<T> IndexMut<Id<T>> for Arena<T> {
    fn index_mut(&mut self, id: Id<T>) -> &mut T {
        &mut self.data[(id.raw as usize)]
    }
}

impl<T> Index<IdRange<T>> for Arena<T> {
    type Output = [T];
    fn index(&self, range: IdRange<T>) -> &[T] {
        let start = range.range.start as usize;
        let end = range.range.end as usize;
        &self.data[start..end]
    }
}

impl<T> FromIterator<T> for Arena<T> {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = T>,
    {
        Arena { data: Vec::from_iter(iter) }
    }
}
