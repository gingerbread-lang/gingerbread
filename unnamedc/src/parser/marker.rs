pub(super) struct Marker {
    pos: usize,
}

impl Marker {
    pub(super) fn new(pos: usize) -> Self {
        Self { pos }
    }
}
