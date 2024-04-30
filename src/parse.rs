use chumsky::{
    error::{Error as ChumskyError, Rich},
    extra::Full,
    input::{SpannedInput, WithContext},
    primitive::{choice, custom, just},
    recursive::recursive,
    util::Maybe,
};

type Tokens<'src> = WithContext<Span, SpannedInput<Token, Span, &'src [(Token, Span)]>>;
type Extras<'src, 'bump> = Full<Rich<'src, Token, Span>, State<'bump>, Context>;
pub trait Parser<'src, 'bump: 'src, O: 'bump> =
    chumsky::Parser<'src, Tokens<'src>, O, Extras<'src, 'bump>> + Clone;
trait IterParser<'src, 'bump: 'src, O: 'bump> =
    chumsky::IterParser<'src, Tokens<'src>, O, Extras<'src, 'bump>> + Clone;

use crate::{
    span::Span,
    token::{Ident, Token},
};

pub struct State<'bump> {
    pub bump: &'bump bumpalo::Bump,
    pub func: Ident,
}

#[derive(Clone, Copy, Default)]
pub struct Context {}

trait IterParserExt<'src, 'bump: 'src, O: 'bump>: IterParser<'src, 'bump, O> {
    fn collect_bump_vec(self) -> impl Parser<'src, 'bump, bumpalo::collections::Vec<'bump, O>> {
        let vec = custom::<_, _, _, Extras>(|input| {
            Ok(bumpalo::collections::Vec::new_in(input.state().bump))
        });

        vec.foldl(self, |mut vec, item| {
            vec.push(item);
            vec
        })
    }
}

impl<'src, 'bump: 'src, O: 'bump, P> IterParserExt<'src, 'bump, O> for P where
    P: IterParser<'src, 'bump, O>
{
}

pub fn parse_file<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, FileNode<'bump>> {
    parse_func()
        .repeated()
        .collect_bump_vec()
        .map(|vec| FileNode(vec.into_bump_slice()))
}

fn parse_func<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, FuncNode<'bump>> {
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
) -> impl Parser<'src, 'bump, Ident> {
    custom::<_, _, _, Extras>(move |input| {
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

fn parse_ident_or_infer<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, Option<Ident>> {
    parse_ident().map(Some).or(just(Token::Infer).map(|_| None))
}

fn parse_ident<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, Ident> {
    custom::<_, _, _, Extras>(|input| {
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

fn parse_int<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, i64> {
    custom::<_, _, _, Extras>(|input| {
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

fn parse_expr<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, Expr<'bump>> {
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
    p: impl Parser<'src, 'bump, O>,
) -> impl Parser<'src, 'bump, &'bump [O]> {
    just(Token::Open)
        .ignore_then(p.repeated().collect_bump_vec().map(|v| v.into_bump_slice()))
        .then_ignore(just(Token::Close))
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct FileNode<'bump>(&'bump [FuncNode<'bump>]);
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

#[cfg(feature = "_debugging")]
impl dbg_pls::DebugWith<lasso::Rodeo<Ident>> for Ident {
    fn fmt(&self, with: &lasso::Rodeo<Ident>, f: dbg_pls::Formatter<'_>) {
        with.resolve(self).fmt(&(), f)
    }
}
