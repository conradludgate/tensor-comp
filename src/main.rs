use ariadne::{Label, Report, Span as ISpan};
use bumpalo::Bump;
use chumsky::{input::Input, Parser};
use logos::Logos;

use crate::{
    file::FileCache,
    parse::{parse_file, State},
    span::Span,
    token::Token,
};

mod file;
mod parse;
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
                Report::build(ariadne::ReportKind::Error, file, span.start())
                    .with_label(Label::new(span).with_message(e))
                    .finish(),
            ),
        }
    }
    let eoi = Span::new(file, src.len()..src.len());
    if lex.extras.depth != 0 {
        errors.push(
            Report::build(ariadne::ReportKind::Error, file, eoi.start())
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

    let mut state = State {
        bump: &Bump::new(),
        func: lex.extras.idents.get_or_intern_static("func"),
    };

    let tokens = tokens.as_slice().spanned(eoi).with_context(file);
    let file = parse_file().parse_with_state(tokens, &mut state).unwrap();
    dbg_pls::color!(file);
}
