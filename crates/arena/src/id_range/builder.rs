use crate::{Id, IdRange};

pub struct IdRangeBuilder<T>(pub(super) Repr<T>);

impl<T> IdRangeBuilder<T> {
    pub fn include(&mut self, elem: Id<T>) {
        self.0 = match self.0 {
            Repr::Empty => Repr::OnlyFirst(elem),
            Repr::OnlyFirst(first) => Repr::FirstAndLast { first, last: elem },
            Repr::FirstAndLast { first, .. } => Repr::FirstAndLast { first, last: elem },
        };
    }

    pub fn build(self) -> IdRange<T> {
        match self.0 {
            Repr::Empty => IdRange::default(),
            Repr::OnlyFirst(first) => IdRange::new(first..=first),
            Repr::FirstAndLast { first, last } => IdRange::new(first..=last),
        }
    }
}

pub(super) enum Repr<T> {
    Empty,
    OnlyFirst(Id<T>),
    FirstAndLast { first: Id<T>, last: Id<T> },
}
