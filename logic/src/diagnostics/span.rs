use super::position::Position;

#[derive(Copy, Clone, Debug, Eq)]
pub struct Span {
    start: Position,
    stop: Position,
}

impl PartialEq for Span {
    fn eq(&self, _: &Self) -> bool {
        // true for now, I may adjust this later
        true
    }
}

impl Span {
    pub fn new(start: Position, stop: Position) -> Self {
        Self { start, stop }
    }
}
