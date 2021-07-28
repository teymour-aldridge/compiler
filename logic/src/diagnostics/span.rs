use super::position::Position;

#[derive(Copy, Clone, Debug)]
pub struct Span {
    start: Position,
    stop: Position,
}

impl Span {
    pub fn new(start: Position, stop: Position) -> Self {
        Self { start, stop }
    }
}
