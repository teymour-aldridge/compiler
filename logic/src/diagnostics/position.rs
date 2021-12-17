#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
pub struct Position {
    pub(crate) column: usize,
    pub(crate) line: usize,
    pub(crate) index: usize,
}
