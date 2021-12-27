#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
/// A position in a file, containing the line number, column number and offset from the start of the
/// file.
pub struct Position {
    pub(crate) column: usize,
    pub(crate) line: usize,
    pub(crate) index: usize,
}
