use std::cell::Cell;
use std::rc::Rc;

pub(crate) struct ExpectedNameGuard {
    current_expected_group: Rc<Cell<Option<usize>>>,
}

impl ExpectedNameGuard {
    pub(super) fn new(current_expected_group: Rc<Cell<Option<usize>>>) -> Self {
        Self { current_expected_group }
    }
}

impl Drop for ExpectedNameGuard {
    fn drop(&mut self) {
        self.current_expected_group.set(None);
    }
}
