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
