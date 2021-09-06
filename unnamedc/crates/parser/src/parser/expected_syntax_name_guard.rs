use std::cell::Cell;
use std::rc::Rc;

pub(crate) struct ExpectedSyntaxNameGuard {
    is_named_expected_syntax_active: Rc<Cell<bool>>,
}

impl ExpectedSyntaxNameGuard {
    pub(super) fn new(is_named_expected_syntax_active: Rc<Cell<bool>>) -> Self {
        Self { is_named_expected_syntax_active }
    }
}

impl Drop for ExpectedSyntaxNameGuard {
    fn drop(&mut self) {
        self.is_named_expected_syntax_active.set(false);
    }
}
