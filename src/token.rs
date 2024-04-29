use core::fmt;
use std::{num::NonZeroU32, str::FromStr};

use lasso::Rodeo;
use logos::{Lexer, Logos};

pub struct Extra {
    pub depth: usize,
    pub idents: Rodeo<Ident>,
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
pub enum Error {
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
pub enum Token {
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
pub struct Ident(NonZeroU32);

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
