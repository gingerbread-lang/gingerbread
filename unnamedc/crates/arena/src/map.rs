use crate::Id;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ArenaMap<I, V> {
    data: Vec<Option<V>>,
    phantom: PhantomData<I>,
}

impl<K, V> fmt::Debug for ArenaMap<Id<K>, V>
where
    V: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ArenaMap ")?;
        f.debug_map()
            .entries(self.data.iter().enumerate().filter_map(|(idx, elem)| {
                elem.as_ref().map(|elem| (Id::<K>::from_raw(idx as u32), elem))
            }))
            .finish()
    }
}

impl<K, V> ArenaMap<Id<K>, V> {
    pub fn insert(&mut self, id: Id<K>, v: V) {
        let idx = id.raw as usize;

        self.data.resize_with((idx + 1).max(self.data.len()), || None);
        self.data[idx] = Some(v);
    }

    pub fn get(&self, id: Id<K>) -> Option<&V> {
        self.data.get(id.raw as usize).and_then(|it| it.as_ref())
    }

    pub fn get_mut(&mut self, id: Id<K>) -> Option<&mut V> {
        self.data.get_mut(id.raw as usize).and_then(|it| it.as_mut())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.data.iter().filter_map(|o| o.as_ref())
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.data.iter_mut().filter_map(|o| o.as_mut())
    }

    pub fn iter(&self) -> impl Iterator<Item = (Id<K>, &V)> {
        self.data
            .iter()
            .enumerate()
            .filter_map(|(idx, o)| Some((Id::from_raw(idx as u32), o.as_ref()?)))
    }
}

impl<V, K> Index<Id<K>> for ArenaMap<Id<K>, V> {
    type Output = V;
    fn index(&self, id: Id<K>) -> &V {
        self.data[id.raw as usize].as_ref().unwrap()
    }
}

impl<V, K> IndexMut<Id<K>> for ArenaMap<Id<K>, V> {
    fn index_mut(&mut self, id: Id<K>) -> &mut V {
        self.data[id.raw as usize].as_mut().unwrap()
    }
}

impl<V, K> Default for ArenaMap<Id<K>, V> {
    fn default() -> Self {
        ArenaMap { data: Vec::new(), phantom: PhantomData }
    }
}
