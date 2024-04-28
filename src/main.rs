use core::fmt;
use std::{
    collections::HashMap,
    iter::Sum,
    num::{NonZeroU16, NonZeroU32},
    ops::Range,
};

use ariadne::{Cache, Label, Report, Source};
use lasso::Rodeo;
use logos::{Lexer, Logos};
use typed_arena::Arena;

#[derive(Default)]
struct Extra {
    depth: usize,
}

#[derive(Debug, PartialEq, Clone, Default)]
enum Error {
    UnbalancedParen,
    #[default]
    UnexpectedCharacter,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnbalancedParen => f.write_str("unbalanced parentheses"),
            Error::UnexpectedCharacter => f.write_str("unexpected character"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(extras = Extra)]
#[logos(error = Error)]
enum Token {
    #[token("(", depth_inc)]
    Open(usize),

    #[token(")", depth_dec)]
    Close(usize),

    #[regex("_", priority = 3)]
    Infer,

    #[regex(r"[0-9]+\.[0-9]+", priority = 2)]
    Float,

    #[regex(r"[0-9]+", priority = 1)]
    Int,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident,
}

fn depth_inc(lex: &mut Lexer<Token>) -> usize {
    let depth = lex.extras.depth;
    lex.extras.depth += 1;
    depth
}

fn depth_dec(lex: &mut Lexer<Token>) -> Result<usize, Error> {
    let depth = lex
        .extras
        .depth
        .checked_sub(1)
        .ok_or(Error::UnbalancedParen)?;
    lex.extras.depth = depth;
    Ok(depth)
}

#[derive(Debug)]
enum SyntaxNode<'arena> {
    Float(f64, Span),
    Int(i64, Span),
    Infer(Span),
    Ident(Ident, Span),
    List(&'arena [SyntaxNode<'arena>], Span),
}

impl SyntaxNode<'_> {
    fn span(&self) -> Span {
        match self {
            SyntaxNode::Float(_, span) => *span,
            SyntaxNode::Int(_, span) => *span,
            SyntaxNode::Infer(span) => *span,
            SyntaxNode::Ident(_, span) => *span,
            SyntaxNode::List(_, span) => *span,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Span(File, u32, u32);

impl Span {
    pub fn new(file: File, span: Range<usize>) -> Self {
        Self(
            file,
            span.start.try_into().expect("file too large"),
            span.end.try_into().expect("file too large"),
        )
    }

    fn join(self, other: Self) -> Self {
        assert_eq!(self.0, other.0);
        let min = self.1.min(other.1);
        let max = self.2.max(other.2);
        Self(self.0, min, max)
    }
}

impl Sum<Span> for Span {
    fn sum<I: Iterator<Item = Span>>(iter: I) -> Self {
        iter.into_iter().reduce(Self::join).unwrap()
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Ident(NonZeroU32);
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
struct File(NonZeroU16);

unsafe impl lasso::Key for Ident {
    fn into_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        int.checked_add(1)
            .and_then(|x| u32::try_from(x).ok())
            .and_then(NonZeroU32::new)
            .map(Self)
    }
}
unsafe impl lasso::Key for File {
    fn into_usize(self) -> usize {
        self.0.get() as usize - 1
    }

    fn try_from_usize(int: usize) -> Option<Self> {
        int.checked_add(1)
            .and_then(|x| u16::try_from(x).ok())
            .and_then(NonZeroU16::new)
            .map(Self)
    }
}

/// A [`Cache`] that fetches [`Source`]s using the provided function.
#[derive(Debug, Clone)]
pub struct FileCache<I: AsRef<str>> {
    files: Rodeo<File>,
    sources: HashMap<File, Source<I>>,
}

impl<I: AsRef<str>> Default for FileCache<I> {
    fn default() -> Self {
        Self {
            files: Rodeo::new(),
            sources: Default::default(),
        }
    }
}

impl<I: AsRef<str>> FileCache<I> {
    fn insert(&mut self, path: &str, source: impl Into<Source<I>>) -> (File, &str) {
        let file = self.files.get_or_intern(path);
        let source = self.sources.entry(file).or_insert(source.into());
        (file, source.text())
    }
}

impl<I: AsRef<str>> Cache<File> for FileCache<I> {
    type Storage = I;

    fn fetch(
        &mut self,
        id: &File,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new("missing file") as Box<_>)
    }

    fn display<'a>(&self, id: &'a File) -> Option<Box<dyn std::fmt::Display + 'a>> {
        self.files
            .try_resolve(id)
            .map(|x| Box::new(x.to_string()) as Box<_>)
    }
}

fn main() {
    let mut errors = vec![];

    let mut files = FileCache::default();

    let (file, src) = files.insert(
        "example.tensor",
        "
(def mnist
    (state batch input)
    (
        (join (dense_state _ _) (dense_state _ _) (dense_state _ _))
        natural
        (tensor batch 784 _)
    )
    (
        (split state (layer1 layer2 layer3) (
            input
            (dense layer1 _)
            (relu _)
            (dense layer2 _)
            (relu _)
            (dense layer3 _)
            (sigmoid _)
        ))
    )
)
    ",
    );

    let mut lex = Token::lexer(src);

    let syntax_tree_arena = Arena::<SyntaxNode>::new();
    let mut len_stack = vec![];
    let mut syntax_tree_stack = vec![];
    let mut interner = Rodeo::<Ident>::new();

    while let Some(token) = lex.next() {
        let span = Span::new(file, lex.span());
        match token {
            Ok(Token::Open(_)) => len_stack.push((syntax_tree_stack.len(), span)),
            Ok(Token::Close(_)) => {
                let (n, open_span) = len_stack.pop().unwrap();
                let list = syntax_tree_arena.alloc_extend(syntax_tree_stack.drain(n..));
                let list_span = std::iter::once(open_span)
                    .chain(list.iter().map(|x| x.span()))
                    .chain(std::iter::once(span))
                    .sum();
                syntax_tree_stack.push(SyntaxNode::List(&*list, list_span));
            }
            Ok(Token::Ident) => {
                syntax_tree_stack
                    .push(SyntaxNode::Ident(interner.get_or_intern(lex.slice()), span));
            }
            Ok(Token::Infer) => {
                syntax_tree_stack
                    .push(SyntaxNode::Ident(interner.get_or_intern(lex.slice()), span));
            }
            Ok(Token::Int) => match lex.slice().parse() {
                Ok(int) => syntax_tree_stack.push(SyntaxNode::Int(int, span)),
                Err(e) => errors.push(
                    Report::build(ariadne::ReportKind::Error, file, 0)
                        .with_label(Label::new(span).with_message(e))
                        .finish(),
                ),
            },
            Ok(Token::Float) => match lex.slice().parse() {
                Ok(float) => syntax_tree_stack.push(SyntaxNode::Float(float, span)),
                Err(e) => errors.push(
                    Report::build(ariadne::ReportKind::Error, file, 0)
                        .with_label(Label::new(span).with_message(e))
                        .finish(),
                ),
            },
            Err(e) => errors.push(
                Report::build(ariadne::ReportKind::Error, file, 0)
                    .with_label(Label::new(span).with_message(e))
                    .finish(),
            ),
        }
    }

    let file_list = syntax_tree_arena.alloc_extend(syntax_tree_stack.drain(..));
    let file_span = file_list.iter().map(|x| x.span()).sum();
    let file_list = SyntaxNode::List(&*file_list, file_span);
    dbg!(file_list);

    if !errors.is_empty() {
        for error in errors {
            error.eprint(&mut files).unwrap();
        }
    }
}
