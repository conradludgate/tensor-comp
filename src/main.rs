use core::fmt;

use ariadne::{Label, Report, Source};
use logos::{Lexer, Logos};

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

    // #[regex("[0-9]+")]
    // Number,
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

fn main() {
    // let mut tokens = vec![];
    let mut errors = vec![];

    let src = Source::from("(hello world (1foo bar))");
    let file = "example.tensor";

    let mut lex = Token::lexer(src.text());
    while let Some(token) = lex.next() {
        match token {
            Ok(token) => {
                dbg!(token);
            }
            Err(e) => errors.push(
                Report::build(ariadne::ReportKind::Error, file, 0)
                    .with_label(Label::new((file, lex.span())).with_message(e))
                    .finish(),
            ),
        }
    }

    if !errors.is_empty() {
        for error in errors {
            error.eprint((file, src.clone())).unwrap();
        }
    }
}
