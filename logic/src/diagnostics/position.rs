#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Position {
    pub(crate) column: usize,
    pub(crate) line: usize,
    pub(crate) index: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            column: 0,
            line: 0,
            index: 0,
        }
    }
}
