//! Utilities for mapping source code to intermediate representations.

#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub file_id: u32,
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Convenience method to construct a [Span] with [usize]s instead of [u32].
    ///
    /// # Panics
    /// Panics if *file_id*, *start* or *end* exceed `u32::MAX`.
    #[inline]
    pub fn new(file_id: usize, start: usize, end: usize) -> Self {
        Self {
            file_id: file_id as u32,
            start: start as u32,
            end: end as u32,
        }
    }

    /// Returns the ID of the file which the [Span] originated in.
    #[inline]
    pub fn file_id(&self) -> usize {
        self.file_id as usize
    }

    /// Returns the starting position of the span.
    #[inline]
    pub fn start(&self) -> usize {
        self.start as usize
    }

    /// Returns the ending position of the span.
    #[inline]
    pub fn end(&self) -> usize {
        self.end as usize
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start()..span.end()
    }
}
