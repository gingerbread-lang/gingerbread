use crate::{Id, IdRange};

pub struct IdRangeBuilder<T>(pub(super) IdRangeBuilderRepr<T>);

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

pub(super) enum IdRangeBuilderRepr<T> {
    Empty,
    OnlyFirst(Id<T>),
    FirstAndLast { first: Id<T>, last: Id<T> },
}
