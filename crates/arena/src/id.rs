use std::fmt;
use std::marker::PhantomData;

pub struct Id<T> {
    pub(crate) raw: u32,
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
    pub(crate) fn from_raw(raw: u32) -> Self {
        Self { raw, phantom: PhantomData }
    }

    pub fn to_raw(self) -> u32 {
        self.raw
    }
}
