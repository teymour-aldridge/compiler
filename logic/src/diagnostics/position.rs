#[derive(Copy, Clone, Debug)]
pub struct Position {
    pub(crate) column: usize,
    pub(crate) line: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self { column: 0, line: 0 }
    }
}
