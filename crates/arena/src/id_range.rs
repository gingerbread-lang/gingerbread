pub(crate) mod builder;
use self::builder::{IdRangeBuilder, Repr};

use crate::Id;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Range, RangeInclusive};

pub struct IdRange<T> {
    pub(crate) range: Range<u32>,
    phantom: PhantomData<T>,
}

impl<T> IdRange<T> {
    pub fn new(range: RangeInclusive<Id<T>>) -> Self {
        Self { range: range.start().to_raw()..range.end().to_raw() + 1, phantom: PhantomData }
    }

    pub fn builder() -> IdRangeBuilder<T> {
        IdRangeBuilder(Repr::Empty)
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
