use std::ops::Range;

use crate::file::File;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span(File, u32, u32);

impl Span {
    pub fn new(file: File, span: Range<usize>) -> Self {
        Self(
            file,
            span.start.try_into().expect("file too large"),
            span.end.try_into().expect("file too large"),
        )
    }
}

impl chumsky::span::Span for Span {
    type Context = File;

    type Offset = u32;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self(context, range.start, range.end)
    }

    fn context(&self) -> Self::Context {
        self.0
    }

    fn start(&self) -> Self::Offset {
        self.1
    }

    fn end(&self) -> Self::Offset {
        self.2
    }
}

impl ariadne::Span for Span {
    type SourceId = File;

    fn source(&self) -> &Self::SourceId {
        &self.0
    }

    fn start(&self) -> usize {
        self.1 as usize
    }

    fn end(&self) -> usize {
        self.2 as usize
    }
}
