use std::num::NonZeroU32;

use super::parse as ast;
use crate::token::Ident;

pub struct Scoped<'bump> {
    scope: ScopeId,
    stack: Vec<(Ident, ScopeId)>,

    bump: &'bump bumpalo::Bump,
}

impl<'bump> Scoped<'bump> {
    pub fn new(bump: &'bump bumpalo::Bump) -> Self {
        Self {
            scope: ScopeId(NonZeroU32::new(1).unwrap()),
            stack: vec![],
            bump,
        }
    }

    pub fn map_file(&mut self, file: &ast::File) -> File<'bump> {
        let mut funcs = bumpalo::collections::Vec::new_in(self.bump);
        for func in file.0 {
            funcs.push(self.map_func(func))
        }
        File(funcs.into_bump_slice())
    }
    fn map_func(&mut self, func: &ast::Func) -> Func<'bump> {
        let sp = self.stack.len();

        let mut args = bumpalo::collections::Vec::new_in(self.bump);

        let name_scope = self.scope.next();
        self.stack.push((func.name, name_scope));
        let name = ScopedIdent(func.name, Some(name_scope));

        for ident in func.args {
            let scope = self.scope.next();
            self.stack.push((*ident, scope));
            args.push(ScopedIdent(*ident, Some(scope)));
        }
        let args = args.into_bump_slice();

        let mut types = bumpalo::collections::Vec::new_in(self.bump);
        for expr in func.types {
            types.push(self.map_expr(expr))
        }
        let types = types.into_bump_slice();

        let body = self.map_func_body(&func.body);

        self.stack.truncate(sp);

        Func {
            name,
            args,
            types,
            body,
        }
    }

    fn map_func_body(&mut self, body: &ast::FuncBody) -> FuncBody<'bump> {
        let mut e = bumpalo::collections::Vec::new_in(self.bump);
        for expr in body.steps {
            e.push(self.map_expr(expr));
        }
        FuncBody {
            steps: e.into_bump_slice(),
        }
    }

    fn map_expr(&mut self, expr: &ast::Expr) -> Expr<'bump> {
        match expr {
            ast::Expr::Ident(ident) => Expr::Ident(self.map_ident(*ident)),
            ast::Expr::Infer => Expr::Infer,
            ast::Expr::Int(i) => Expr::Int(*i),
            ast::Expr::Call(c) => Expr::Call(self.map_func_call(c)),
        }
    }

    fn map_func_call(&mut self, call: &ast::FuncCall) -> FuncCall<'bump> {
        let sp = self.stack.len();

        let res = match call {
            ast::FuncCall::Split(split) => {
                let arg = self.map_expr(split.arg);

                let mut idents = bumpalo::collections::Vec::new_in(self.bump);
                for ident in split.idents {
                    let scope = self.scope.next();
                    self.stack.push((*ident, scope));
                    idents.push(ScopedIdent(*ident, Some(scope)));
                }
                let idents = idents.into_bump_slice();

                let body = self.map_func_body(&split.body);

                FuncCall::Split(SplitFuncCall {
                    arg: self.bump.alloc(arg),
                    idents,
                    body,
                })
            }
            ast::FuncCall::Arbitrary(exprs) => {
                let mut e = bumpalo::collections::Vec::new_in(self.bump);
                for expr in &**exprs {
                    e.push(self.map_expr(expr));
                }
                FuncCall::Arbitrary(e.into_bump_slice())
            }
        };

        self.stack.truncate(sp);

        res
    }

    fn map_ident(&mut self, ident: Ident) -> ScopedIdent {
        let scope = self
            .stack
            .iter()
            .rev()
            .find(|scoped| scoped.0 == ident)
            .map(|scoped| scoped.1);
        ScopedIdent(ident, scope)
    }
}

#[derive(Clone, Copy)]
pub struct ScopeId(NonZeroU32);

impl ScopeId {
    fn next(&mut self) -> Self {
        let next = self.0;
        self.0 = self.0.checked_add(1).unwrap();
        Self(next)
    }
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct ScopedIdent(Ident, Option<ScopeId>);

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct File<'bump>(pub &'bump [Func<'bump>]);

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct Func<'bump> {
    pub name: ScopedIdent,
    pub args: &'bump [ScopedIdent],
    pub types: &'bump [Expr<'bump>],
    pub body: FuncBody<'bump>,
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub struct FuncBody<'bump> {
    pub steps: &'bump [Expr<'bump>],
}

#[cfg_attr(feature = "_debugging", derive(dbg_pls::DebugPls))]
pub enum Expr<'bump> {
    Ident(ScopedIdent),
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
    pub idents: &'bump [ScopedIdent],
    pub body: FuncBody<'bump>,
}

#[cfg(feature = "_debugging")]
impl<T> dbg_pls::DebugWith<T> for ScopeId {
    fn fmt(&self, with: &T, f: dbg_pls::Formatter<'_>) {
        self.0.fmt(with, f)
    }
}
