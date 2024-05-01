use chumsky::{
    error::{Error as ChumskyError, Rich},
    extra::Full,
    input::{SpannedInput, WithContext},
    primitive::{choice, custom, just},
    recursive::recursive,
    select,
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
    pub split: Ident,
}

#[derive(Clone, Default)]
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

trait ParserExt<'src, 'bump: 'src, O: 'bump>: Parser<'src, 'bump, O> {
    fn alloc(self) -> impl Parser<'src, 'bump, &'bump O> {
        self.repeated()
            .exactly(1)
            .collect_bump_vec()
            .map(|o| o.into_bump_slice().first().expect("exactly one item"))
    }
}

impl<'src, 'bump: 'src, O: 'bump, P> ParserExt<'src, 'bump, O> for P where P: Parser<'src, 'bump, O> {}

pub fn parse_file<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, File<'bump>> {
    parse_func(parse_expr())
        .repeated()
        .collect_bump_vec()
        .map(|vec| File(vec.into_bump_slice()))
}

fn parse_func<'src, 'bump: 'src>(
    expr: impl Parser<'src, 'bump, Expr<'bump>>,
) -> impl Parser<'src, 'bump, Func<'bump>> {
    parse_ident_exact(|s| s.func)
        .ignore_then(parse_ident())
        .then(parse_list(parse_ident()))
        .then(parse_list(expr.clone()))
        .then(parse_func_body(expr))
        .delimited_by(just(Token::Open), just(Token::Close))
        .map(|(((name, args), types), body)| Func {
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

fn parse_ident<'src, 'bump: 'src>() -> impl Parser<'src, 'bump, Ident> {
    select! {
        Token::Ident(ident) => ident,
    }
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
            parse_func_call(expr).map(Expr::Call),
        ))
    })
}

fn parse_func_call<'src, 'bump: 'src>(
    expr: impl Parser<'src, 'bump, Expr<'bump>>,
) -> impl Parser<'src, 'bump, FuncCall<'bump>> {
    choice((
        parse_split_func_call(expr.clone()).map(FuncCall::Split),
        expr.repeated()
            .collect_bump_vec()
            .map(|v| FuncCall::Arbitrary(v.into_bump_slice())),
    ))
    .delimited_by(just(Token::Open), just(Token::Close))
}

fn parse_split_func_call<'src, 'bump: 'src>(
    expr: impl Parser<'src, 'bump, Expr<'bump>>,
) -> impl Parser<'src, 'bump, SplitFuncCall<'bump>> {
    parse_ident_exact(|s| s.split)
        .ignore_then(expr.clone().alloc())
        .then(parse_list(parse_ident()))
        .then(parse_func_body(expr))
        .map(|((arg, idents), body)| SplitFuncCall { arg, idents, body })
}

fn parse_list<'src, 'bump: 'src, O: 'bump>(
    p: impl Parser<'src, 'bump, O>,
) -> impl Parser<'src, 'bump, &'bump [O]> {
    p.repeated()
        .collect_bump_vec()
        .map(|v| v.into_bump_slice())
        .delimited_by(just(Token::Open), just(Token::Close))
}

fn parse_func_body<'src, 'bump: 'src>(
    expr: impl Parser<'src, 'bump, Expr<'bump>>,
) -> impl Parser<'src, 'bump, FuncBody<'bump>> {
    parse_list(expr).map(|steps| FuncBody { steps })
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct File<'bump>(pub &'bump [Func<'bump>]);

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct Func<'bump> {
    pub name: Ident,
    pub args: &'bump [Ident],
    pub types: &'bump [Expr<'bump>],
    pub body: FuncBody<'bump>,
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct FuncBody<'bump> {
    pub steps: &'bump [Expr<'bump>],
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub enum Expr<'bump> {
    Ident(Ident),
    Infer,
    // Float(f64),
    Int(i64),
    Call(FuncCall<'bump>),
}

pub enum FuncCall<'bump> {
    Split(SplitFuncCall<'bump>),
    Arbitrary(&'bump [Expr<'bump>]),
}

#[cfg(feature = "_debugging")]
impl<T> dbg_pls::DebugWith<T> for FuncCall<'_>
where
    Ident: dbg_pls::DebugWith<T>,
{
    fn fmt(&self, with: &T, f: dbg_pls::Formatter<'_>) {
        match self {
            FuncCall::Split(split) => f
                .debug_tuple_struct("Split")
                .field_with(split as _, with)
                .finish(),
            FuncCall::Arbitrary(arbitrary) => f
                .debug_tuple_struct("Arbitrary")
                .field_with(arbitrary as _, with)
                .finish(),
        }
    }
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct SplitFuncCall<'bump> {
    pub arg: &'bump Expr<'bump>,
    pub idents: &'bump [Ident],
    pub body: FuncBody<'bump>,
}

#[cfg(feature = "_debugging")]
impl dbg_pls::DebugWith<lasso::Rodeo<Ident>> for Ident {
    fn fmt(&self, with: &lasso::Rodeo<Ident>, f: dbg_pls::Formatter<'_>) {
        with.resolve(self).fmt(&(), f)
    }
}
