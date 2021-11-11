mod map;
pub use map::ArenaMap;

use std::fmt;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut, Range, RangeInclusive};

pub struct Id<T> {
    raw: u32,
    phantom: PhantomData<fn() -> T>,
}

impl<T> Clone for Id<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Id<T> {}

impl<T> PartialEq for Id<T> {
    fn eq(&self, other: &Id<T>) -> bool {
        self.raw == other.raw
    }
}
impl<T> Eq for Id<T> {}

impl<T> fmt::Debug for Id<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut type_name = std::any::type_name::<T>();
        if let Some(idx) = type_name.rfind(':') {
            type_name = &type_name[idx + 1..]
        }
        write!(f, "Id::<{}>({})", type_name, self.raw)
    }
}

impl<T> Id<T> {
    fn from_raw(raw: u32) -> Self {
        Self { raw, phantom: PhantomData }
    }
}

pub struct IdRange<T> {
    range: Range<u32>,
    phantom: PhantomData<T>,
}

impl<T> IdRange<T> {
    pub fn new(range: RangeInclusive<Id<T>>) -> Self {
        Self { range: range.start().raw..range.end().raw + 1, phantom: PhantomData }
    }

    pub fn builder() -> IdRangeBuilder<T> {
        IdRangeBuilder(IdRangeBuilderRepr::Empty)
    }

    pub fn len(&self) -> usize {
        self.range.len()
    }

    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }
}

impl<T> Iterator for IdRange<T> {
    type Item = Id<T>;
    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(Id::from_raw)
    }
}

impl<T> DoubleEndedIterator for IdRange<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range.next_back().map(Id::from_raw)
    }
}

impl<T> fmt::Debug for IdRange<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.range.start == 0 && self.range.end == 0 {
            return write!(f, "IdRange::<{}>(0..0)", std::any::type_name::<T>());
        }

        write!(
            f,
            "IdRange::<{}>({}..={})",
            std::any::type_name::<T>(),
            self.range.start,
            self.range.end - 1
        )
    }
}

impl<T> Clone for IdRange<T> {
    fn clone(&self) -> Self {
        Self { range: self.range.clone(), phantom: PhantomData }
    }
}

impl<T> PartialEq for IdRange<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl<T> Eq for IdRange<T> {}

impl<T> Default for IdRange<T> {
    fn default() -> Self {
        Self { range: 0..0, phantom: PhantomData }
    }
}

pub struct IdRangeBuilder<T>(IdRangeBuilderRepr<T>);

impl<T> IdRangeBuilder<T> {
    pub fn include(&mut self, elem: Id<T>) {
        self.0 = match self.0 {
            IdRangeBuilderRepr::Empty => IdRangeBuilderRepr::OnlyFirst(elem),
            IdRangeBuilderRepr::OnlyFirst(first) => {
                IdRangeBuilderRepr::FirstAndLast { first, last: elem }
            }
            IdRangeBuilderRepr::FirstAndLast { first, last: _last } => {
                IdRangeBuilderRepr::FirstAndLast { first, last: elem }
            }
        };
    }

    pub fn build(self) -> IdRange<T> {
        match self.0 {
            IdRangeBuilderRepr::Empty => IdRange::default(),
            IdRangeBuilderRepr::OnlyFirst(first) => IdRange::new(first..=first),
            IdRangeBuilderRepr::FirstAndLast { first, last } => IdRange::new(first..=last),
        }
    }
}

enum IdRangeBuilderRepr<T> {
    Empty,
    OnlyFirst(Id<T>),
    FirstAndLast { first: Id<T>, last: Id<T> },
}

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
        let id = self.next_id();
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

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit();
    }

    fn next_id(&self) -> Id<T> {
        Id::from_raw(self.data.len() as u32)
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
