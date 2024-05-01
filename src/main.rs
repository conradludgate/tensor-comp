#![feature(trait_alias)]

use ariadne::{Label, Report};
use bumpalo::Bump;
use chumsky::{input::Input, span::Span as ISpan, Parser};
use logos::Logos;

use crate::{
    file::FileCache,
    parse::{parse_file, State},
    scope::Scoped,
    span::Span,
    token::Token,
};

mod file;
mod parse;
mod scope;
mod span;
mod token;

fn main() {
    let mut errors = vec![];

    let mut files = FileCache::default();

    let (file, src) = files.insert(
        "example.tensor",
        "
(func mnist (state batch input)
    (
        (join (dense_state _ 16) (dense_state _ 16) (dense_state _ 10))
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
                Report::build(ariadne::ReportKind::Error, file, span.start() as usize)
                    .with_label(Label::new(span).with_message(e))
                    .finish(),
            ),
        }
    }
    let eoi = Span::new(file, src.len()..src.len());
    if lex.extras.depth != 0 {
        errors.push(
            Report::build(ariadne::ReportKind::Error, file, eoi.start() as usize)
                .with_label(Label::new(eoi).with_message(token::Error::UnbalancedParen))
                .finish(),
        )
    }

    if !errors.is_empty() {
        for error in errors {
            error.eprint(&mut files).unwrap();
        }
        return;
    }

    let bump_parse = Bump::new();
    let mut state = State {
        bump: &bump_parse,
        func: lex.extras.idents.get_or_intern_static("func"),
        split: lex.extras.idents.get_or_intern_static("split"),
    };

    let tokens = tokens.as_slice().spanned(eoi).with_context(file);

    let file = match parse_file()
        .parse_with_state(tokens, &mut state)
        .into_result()
    {
        Ok(file) => dbg_pls::color_with!(&lex.extras.idents, file),
        Err(errors) => {
            for error in errors {
                Report::build(
                    ariadne::ReportKind::Error,
                    error.span().context(),
                    error.span().start() as usize,
                )
                .with_label(Label::new(*error.span()).with_message(format!("{error:?}")))
                .finish()
                .eprint(&mut files)
                .unwrap();
            }
            return;
        }
    };

    let scoped_bump = Bump::new();
    let mut scoped = Scoped::new(&scoped_bump);
    let scoped_file = scoped.map_file(&file);
    dbg_pls::color_with!(&lex.extras.idents, scoped_file);
}
