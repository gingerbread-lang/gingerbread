mod map;
pub use map::ArenaMap;

use std::fmt;
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut, Range, RangeInclusive};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RawIdx(u32);

impl From<RawIdx> for u32 {
    fn from(raw: RawIdx) -> u32 {
        raw.0
    }
}

impl From<u32> for RawIdx {
    fn from(idx: u32) -> RawIdx {
        RawIdx(idx)
    }
}

impl fmt::Debug for RawIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Display for RawIdx {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct Idx<T> {
    raw: RawIdx,
    _ty: PhantomData<fn() -> T>,
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Idx<T>) -> bool {
        self.raw == other.raw
    }
}
impl<T> Eq for Idx<T> {}

impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut type_name = std::any::type_name::<T>();
        if let Some(idx) = type_name.rfind(':') {
            type_name = &type_name[idx + 1..]
        }
        write!(f, "Idx::<{}>({})", type_name, self.raw)
    }
}

impl<T> Idx<T> {
    pub fn from_raw(raw: RawIdx) -> Self {
        Idx { raw, _ty: PhantomData }
    }

    pub fn into_raw(self) -> RawIdx {
        self.raw
    }
}

pub struct IdxRange<T> {
    range: Range<u32>,
    _p: PhantomData<T>,
}

impl<T> IdxRange<T> {
    pub fn new(range: Range<Idx<T>>) -> Self {
        Self { range: range.start.into_raw().into()..range.end.into_raw().into(), _p: PhantomData }
    }

    pub fn new_inclusive(range: RangeInclusive<Idx<T>>) -> Self {
        Self {
            range: u32::from(range.start().into_raw())..u32::from(range.end().into_raw()) + 1,
            _p: PhantomData,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.range.is_empty()
    }
}

impl<T> Iterator for IdxRange<T> {
    type Item = Idx<T>;
    fn next(&mut self) -> Option<Self::Item> {
        self.range.next().map(|raw| Idx::from_raw(raw.into()))
    }
}

impl<T> DoubleEndedIterator for IdxRange<T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.range.next_back().map(|raw| Idx::from_raw(raw.into()))
    }
}

impl<T> fmt::Debug for IdxRange<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(&format!("IdRange::<{}>", std::any::type_name::<T>()))
            .field(&self.range)
            .finish()
    }
}

impl<T> Clone for IdxRange<T> {
    fn clone(&self) -> Self {
        Self { range: self.range.clone(), _p: PhantomData }
    }
}

impl<T> PartialEq for IdxRange<T> {
    fn eq(&self, other: &Self) -> bool {
        self.range == other.range
    }
}

impl<T> Eq for IdxRange<T> {}

impl<T> Default for IdxRange<T> {
    fn default() -> Self {
        Self { range: 0..0, _p: PhantomData }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T: fmt::Debug> fmt::Debug for Arena<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt.debug_struct("Arena").field("len", &self.len()).field("data", &self.data).finish()
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

    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let idx = self.next_idx();
        self.data.push(value);
        idx
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (Idx<T>, &T)> + ExactSizeIterator + DoubleEndedIterator {
        self.data.iter().enumerate().map(|(idx, value)| (Idx::from_raw(RawIdx(idx as u32)), value))
    }

    pub fn iter_mut(
        &mut self,
    ) -> impl Iterator<Item = (Idx<T>, &mut T)> + ExactSizeIterator + DoubleEndedIterator {
        self.data
            .iter_mut()
            .enumerate()
            .map(|(idx, value)| (Idx::from_raw(RawIdx(idx as u32)), value))
    }

    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit();
    }

    fn next_idx(&self) -> Idx<T> {
        Idx::from_raw(RawIdx(self.data.len() as u32))
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Arena<T> {
        Arena { data: Vec::new() }
    }
}

impl<T> Index<Idx<T>> for Arena<T> {
    type Output = T;
    fn index(&self, idx: Idx<T>) -> &T {
        let idx = idx.into_raw().0 as usize;
        &self.data[idx]
    }
}

impl<T> IndexMut<Idx<T>> for Arena<T> {
    fn index_mut(&mut self, idx: Idx<T>) -> &mut T {
        let idx = idx.into_raw().0 as usize;
        &mut self.data[idx]
    }
}

impl<T> Index<IdxRange<T>> for Arena<T> {
    type Output = [T];
    fn index(&self, range: IdxRange<T>) -> &[T] {
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
