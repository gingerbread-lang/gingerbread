use crate::Idx;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArenaMap<I, V> {
    v: Vec<Option<V>>,
    _ty: PhantomData<I>,
}

impl<K, V> fmt::Debug for ArenaMap<Idx<K>, V>
where
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ArenaMap ")?;
        f.debug_map()
            .entries(
                self.v.iter().enumerate().filter_map(|(idx, elem)| {
                    elem.as_ref().map(|elem| (Self::from_idx(idx), elem))
                }),
            )
            .finish()
    }
}

impl<K, V> ArenaMap<Idx<K>, V> {
    pub fn insert(&mut self, idx: Idx<K>, t: V) {
        let idx = Self::to_idx(idx);

        self.v.resize_with((idx + 1).max(self.v.len()), || None);
        self.v[idx] = Some(t);
    }

    pub fn get(&self, idx: Idx<K>) -> Option<&V> {
        self.v.get(Self::to_idx(idx)).and_then(|it| it.as_ref())
    }

    pub fn get_mut(&mut self, idx: Idx<K>) -> Option<&mut V> {
        self.v.get_mut(Self::to_idx(idx)).and_then(|it| it.as_mut())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.v.iter().filter_map(|o| o.as_ref())
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.v.iter_mut().filter_map(|o| o.as_mut())
    }

    pub fn iter(&self) -> impl Iterator<Item = (Idx<K>, &V)> {
        self.v.iter().enumerate().filter_map(|(idx, o)| Some((Self::from_idx(idx), o.as_ref()?)))
    }

    fn to_idx(idx: Idx<K>) -> usize {
        u32::from(idx.into_raw()) as usize
    }

    fn from_idx(idx: usize) -> Idx<K> {
        Idx::from_raw((idx as u32).into())
    }
}

impl<V, K> Index<Idx<K>> for ArenaMap<Idx<K>, V> {
    type Output = V;
    fn index(&self, idx: Idx<K>) -> &V {
        self.v[Self::to_idx(idx)].as_ref().unwrap()
    }
}

impl<V, K> IndexMut<Idx<K>> for ArenaMap<Idx<K>, V> {
    fn index_mut(&mut self, idx: Idx<K>) -> &mut V {
        self.v[Self::to_idx(idx)].as_mut().unwrap()
    }
}

impl<V, K> Default for ArenaMap<Idx<K>, V> {
    fn default() -> Self {
        ArenaMap { v: Vec::new(), _ty: PhantomData }
    }
}
