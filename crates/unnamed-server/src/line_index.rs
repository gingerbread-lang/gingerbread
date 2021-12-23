use std::iter;
use std::ops::Index;
use text_size::TextSize;

#[derive(Debug, PartialEq, Default)]
pub(crate) struct LineIndex {
    line_starts: Vec<TextSize>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct LineNr(pub(crate) u32);

#[derive(Debug, Clone, Copy)]
pub(crate) struct ColNr(pub(crate) u32);

impl LineIndex {
    pub(crate) fn new(text: &str) -> Self {
        Self {
            line_starts: iter::once(TextSize::from(0))
                .chain(text.match_indices('\n').map(|(idx, _)| TextSize::from(idx as u32 + 1)))
                .collect(),
        }
    }

    pub(crate) fn line_col(&self, offset: TextSize) -> (LineNr, ColNr) {
        let line = self.line_starts.partition_point(|&it| it <= offset) - 1;
        let line = LineNr(line as u32);

        let line_start_offset = self[line];
        let col = ColNr(u32::from(offset - line_start_offset));

        (line, col)
    }
}

impl Index<LineNr> for LineIndex {
    type Output = TextSize;

    fn index(&self, index: LineNr) -> &Self::Output {
        &self.line_starts[index.0 as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check<const LEN: usize>(text: &str, line_starts: [u32; LEN]) {
        assert_eq!(
            LineIndex::new(text),
            LineIndex { line_starts: line_starts.into_iter().map(TextSize::from).collect() }
        );
    }

    #[test]
    fn empty() {
        check("", [0]);
    }

    #[test]
    fn one() {
        check("\n", [0, 1]);
    }

    #[test]
    fn trailing() {
        check("foo\n", [0, 4]);
    }

    #[test]
    fn two() {
        check("foo\nbar", [0, 4]);
    }
}
