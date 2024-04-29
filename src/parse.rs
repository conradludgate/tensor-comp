use chumsky::{
    error::{Error as ChumskyError, Rich},
    extra::Full,
    input::{SpannedInput, WithContext},
    primitive::{choice, custom, just},
    recursive::recursive,
    util::Maybe,
    IterParser, Parser,
};

use crate::{
    span::Span,
    token::{Ident, Token},
};

type Tokens<'src> = WithContext<Span, SpannedInput<Token, Span, &'src [(Token, Span)]>>;

pub struct State<'bump> {
    pub bump: &'bump bumpalo::Bump,
    pub func: Ident,
}

#[derive(Clone, Copy, Default)]
pub struct Context {}

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

pub fn parse_file<'src, 'bump: 'src>(
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

impl dbg_pls::DebugPls for Ident {
    fn fmt(&self, f: dbg_pls::Formatter<'_>) {
        f.debug_ident("Ident")
    }
}
