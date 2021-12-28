use std::ops::Range;

use super::position::Position;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// A span that contains the indices of the start and positions of the relevant object in the raw
/// source code.
///
/// [Span] also contains the line numbers.
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
/// Relates objects in the compiler to the source code positions from which they came. Note that
/// unlike [IndexOnlySpan] this includes line numbers (this is useful when debugging).
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
    /// Create a new [Span].
    pub fn new(start: Position, stop: Position) -> Self {
        Self { start, stop }
    }

    /// Obtain the range (in terms of string indices) that this item covers. This is returned in the
    /// form `(start_index, stop_index)`.
    pub fn range(&self) -> (usize, usize) {
        (self.start.index, self.stop.index)
    }

    /// Get a reference to the span's start.
    pub fn start(&self) -> Position {
        self.start
    }

    /// Get a reference to the span's stop.
    pub fn stop(&self) -> Position {
        self.stop
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// Contains an item `T` alongside its corresponding source code location.
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
    /// Construct a new spanned object.
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

/// An object whose [Span] can be obtained.
pub trait HasSpan {
    fn span(&self) -> Span;
}
