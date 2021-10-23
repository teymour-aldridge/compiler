use std::ops::Range;

use super::position::Position;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct IndexOnlySpan {
    start: usize,
    stop: usize,
}

impl IndexOnlySpan {
    pub fn new(start: usize, stop: usize) -> Self {
        debug_assert!(stop >= start);
        Self { start, stop }
    }

    pub fn range(&self) -> Range<usize> {
        self.start..self.stop
    }
}

impl From<Span> for IndexOnlySpan {
    fn from(s: Span) -> Self {
        Self::new(s.start.index, s.stop.index)
    }
}

#[derive(Copy, Clone, Debug, Eq)]
pub struct Span {
    #[allow(unused)]
    start: Position,
    #[allow(unused)]
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

    pub fn range(&self) -> (usize, usize) {
        (self.start.index, self.stop.index)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Spanned<T> {
    pub(crate) span: Span,
    pub(crate) token: T,
}

impl<T> HasSpan for Spanned<T> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, token: T) -> Self {
        Self { span, token }
    }
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}
