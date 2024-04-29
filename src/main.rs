use core::fmt;
use std::{
    collections::HashMap,
    num::{NonZeroU16, NonZeroU32},
    ops::Range,
    str::FromStr,
};

use ariadne::{Cache, Label, Report, Source};
use bumpalo::Bump;
use chumsky::{
    error::{Error as ChumskyError, Rich},
    extra::Full,
    input::{Input, SpannedInput, WithContext},
    primitive::{choice, custom, just},
    recursive::recursive,
    util::Maybe,
    IterParser, Parser,
};
use lasso::Rodeo;
use logos::{Lexer, Logos};

struct Extra {
    depth: usize,
    idents: Rodeo<Ident>,
}

impl Default for Extra {
    fn default() -> Self {
        Self {
            depth: 0,
            idents: Rodeo::new(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
enum Error {
    UnbalancedParen,
    Overflow,
    #[default]
    UnexpectedCharacter,
}

impl From<std::num::ParseIntError> for Error {
    fn from(value: std::num::ParseIntError) -> Self {
        match value.kind() {
            std::num::IntErrorKind::PosOverflow => Self::Overflow,
            std::num::IntErrorKind::NegOverflow => Self::Overflow,
            _ => Self::UnexpectedCharacter,
        }
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(_: std::num::ParseFloatError) -> Self {
        Self::Overflow
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UnbalancedParen => f.write_str("unbalanced parentheses"),
            Error::Overflow => f.write_str("number was too large to be represented"),
            Error::UnexpectedCharacter => f.write_str("unexpected character"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")] // Ignore this regex pattern between tokens
#[logos(extras = Extra)]
#[logos(error = Error)]
enum Token {
    #[token("(", depth_inc)]
    Open,

    #[token(")", depth_dec)]
    Close,

    #[regex("_", priority = 3)]
    Infer,

    #[regex(r"[0-9]+\.[0-9]+", parse, priority = 2)]
    Float(f64),

    #[regex(r"[0-9]+", parse, priority = 1)]
    Int(i64),

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", ident)]
    Ident(Ident),
}

fn depth_inc(lex: &mut Lexer<Token>) {
    lex.extras.depth += 1;
}

fn depth_dec(lex: &mut Lexer<Token>) -> Result<(), Error> {
    lex.extras.depth = lex
        .extras
        .depth
        .checked_sub(1)
        .ok_or(Error::UnbalancedParen)?;
    Ok(())
}

fn ident(lex: &mut Lexer<Token>) -> Ident {
    lex.extras.idents.get_or_intern(lex.slice())
}

fn parse<P: FromStr>(lex: &mut Lexer<Token>) -> Result<P, Error>
where
    Error: From<P::Err>,
{
    Ok(lex.slice().parse()?)
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

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
struct Ident(NonZeroU32);

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
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
(func mnist (state batch input)
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
    let mut tokens = vec![];
    while let Some(token) = lex.next() {
        let span = Span::new(file, lex.span());
        match token {
            Ok(token) => tokens.push((token, span)),
            Err(e) => errors.push(
                Report::build(ariadne::ReportKind::Error, file, span.1 as usize)
                    .with_label(Label::new(span).with_message(e))
                    .finish(),
            ),
        }
    }

    if !errors.is_empty() {
        for error in errors {
            error.eprint(&mut files).unwrap();
        }
        return;
    }

    let mut state = State {
        bump: &Bump::new(),
        func: lex.extras.idents.get_or_intern_static("func"),
    };

    let eoi = Span(file, src.len() as u32, src.len() as u32);
    let tokens = tokens.as_slice().spanned(eoi).with_context(file);
    let file = parse_file().parse_with_state(tokens, &mut state).unwrap();
    dbg_pls::color!(file);
}

type Tokens<'src> = WithContext<Span, SpannedInput<Token, Span, &'src [(Token, Span)]>>;

struct State<'bump> {
    bump: &'bump bumpalo::Bump,
    func: Ident,
}

#[derive(Clone, Copy, Default)]
struct Context {}

type E<'src, 'bump> = Full<Rich<'src, Token, Span>, State<'bump>, Context>;

trait IterParserExt<'src, 'bump: 'src, O: 'bump>:
    IterParser<'src, Tokens<'src>, O, E<'src, 'bump>> + Clone
{
    fn collect_bump_vec(
        self,
    ) -> impl Parser<'src, Tokens<'src>, bumpalo::collections::Vec<'bump, O>, E<'src, 'bump>> + Clone
    {
        let vec =
            custom::<_, _, _, E>(|input| Ok(bumpalo::collections::Vec::new_in(input.state().bump)));

        vec.foldl(self, |mut vec, item| {
            vec.push(item);
            vec
        })
    }
}

impl<'src, 'bump: 'src, O: 'bump, P> IterParserExt<'src, 'bump, O> for P where
    P: IterParser<'src, Tokens<'src>, O, E<'src, 'bump>> + Clone
{
}

fn parse_file<'src, 'bump: 'src>(
) -> impl Parser<'src, Tokens<'src>, FileNode<'bump>, E<'src, 'bump>> + Clone {
    parse_func()
        .repeated()
        .collect_bump_vec()
        .map(|vec| FileNode(vec.into_bump_slice()))
}

fn parse_func<'src, 'bump: 'src>(
) -> impl Parser<'src, Tokens<'src>, FuncNode<'bump>, E<'src, 'bump>> + Clone {
    just(Token::Open)
        .ignore_then(parse_ident_exact(|s| s.func))
        .ignore_then(parse_ident())
        .then(parse_list(parse_ident_or_infer()))
        .then(parse_list(parse_expr()))
        .then(parse_expr())
        .then_ignore(just(Token::Close))
        .map(|(((name, args), types), body)| FuncNode {
            name,
            args,
            types,
            body,
        })
}

fn parse_ident_exact<'src, 'bump: 'src>(
    ident_from_state: impl for<'a> Fn(&'a State) -> Ident + Clone,
) -> impl Parser<'src, Tokens<'src>, Ident, E<'src, 'bump>> + Clone {
    custom::<_, _, _, E>(move |input| {
        let expected = ident_from_state(input.state());
        let before = input.offset();
        match input.next() {
            Some(Token::Ident(ident)) if ident == expected => Ok(ident),
            token => {
                let span = input.span_since(before);
                Err(<Rich<_, _, _> as ChumskyError<Tokens>>::expected_found(
                    [Some(Maybe::Val(Token::Ident(expected)))],
                    token.map(Maybe::Val),
                    span,
                ))
            }
        }
    })
}

fn parse_ident_or_infer<'src, 'bump: 'src>(
) -> impl Parser<'src, Tokens<'src>, Option<Ident>, E<'src, 'bump>> + Clone {
    parse_ident().map(Some).or(just(Token::Infer).map(|_| None))
}

fn parse_ident<'src, 'bump: 'src>() -> impl Parser<'src, Tokens<'src>, Ident, E<'src, 'bump>> + Clone
{
    custom::<_, _, _, E>(|input| {
        let before = input.offset();
        match input.next() {
            Some(Token::Ident(ident)) => Ok(ident),
            None => {
                let span = input.span_since(before);
                Err(Rich::custom(span, "found end of input expected identifier"))
            }
            Some(token) => {
                let span = input.span_since(before);
                Err(Rich::custom(
                    span,
                    format!("found {token:?} expected identifier"),
                ))
            }
        }
    })
}

fn parse_int<'src, 'bump: 'src>() -> impl Parser<'src, Tokens<'src>, i64, E<'src, 'bump>> + Clone {
    custom::<_, _, _, E>(|input| {
        let before = input.offset();
        match input.next() {
            Some(Token::Int(int)) => Ok(int),
            None => {
                let span = input.span_since(before);
                Err(Rich::custom(span, "found end of input expected integer"))
            }
            Some(token) => {
                let span = input.span_since(before);
                Err(Rich::custom(
                    span,
                    format!("found {token:?} expected integer"),
                ))
            }
        }
    })
}

fn parse_expr<'src, 'bump: 'src>(
) -> impl Parser<'src, Tokens<'src>, Expr<'bump>, E<'src, 'bump>> + Clone {
    recursive(|expr| {
        choice((
            parse_ident().map(Expr::Ident),
            just(Token::Infer).map(|_| Expr::Infer),
            parse_int().map(Expr::Int),
            parse_list(expr).map(Expr::List),
        ))
    })
}

fn parse_list<'src, 'bump: 'src, O: 'bump>(
    p: impl Parser<'src, Tokens<'src>, O, E<'src, 'bump>> + Clone,
) -> impl Parser<'src, Tokens<'src>, &'bump [O], E<'src, 'bump>> + Clone {
    just(Token::Open)
        .ignore_then(p.repeated().collect_bump_vec().map(|v| v.into_bump_slice()))
        .then_ignore(just(Token::Close))
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
struct FileNode<'bump>(&'bump [FuncNode<'bump>]);
#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
struct FuncNode<'bump> {
    name: Ident,
    args: &'bump [Option<Ident>],
    types: &'bump [Expr<'bump>],
    body: Expr<'bump>,
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
enum Expr<'bump> {
    Ident(Ident),
    Infer,
    // Float(f64),
    Int(i64),
    List(&'bump [Expr<'bump>]),
}

impl dbg_pls::DebugPls for Ident {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_ident("Ident")
    }
}
