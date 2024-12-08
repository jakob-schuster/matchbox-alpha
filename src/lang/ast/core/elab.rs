use matcher::Matcher;

use crate::{lang::ast, util::Arena};

use super::*;

#[derive(Debug)]
pub enum ElabError {}
pub trait Elaborate<'a, F> {
    fn elab(f: &'a F, arena: &'a Arena) -> Result<Self, ElabError>
    where
        Self: Sized;
}

impl<'a> Elaborate<'a, ast::Prog> for Prog<'a> {
    fn elab(f: &'a ast::Prog, arena: &'a Arena) -> Result<Self, ElabError> {
        Ok(Prog {
            stmt: Stmt::elab(&f.stmt, arena)?,
        })
    }
}

fn recursive_bind<'a>(binds: HashMap<Id, Exp<'a>>, stmts: &Vec<Stmt<'a>>) -> Vec<Stmt<'a>> {
    let stmts = stmts.clone();
    let stmts_new = stmts
        .iter()
        .map(|stmt| {
            binds
                .iter()
                .fold(stmt.clone(), |stmt, (id, val)| stmt.subs(id, val))
        })
        .collect_vec();

    if !stmts.eq(&stmts_new) {
        // go again!
        recursive_bind(binds, &stmts_new)
    } else {
        // no need to go again
        stmts_new
    }
}

fn separate_scope<'a>(
    stmts: &'a Vec<ast::Stmt>,
    arena: &'a Arena,
) -> Result<(HashMap<Id, Exp<'a>>, Vec<Stmt<'a>>), ElabError> {
    let mut map: HashMap<Id, Exp> = HashMap::new();
    let mut stmts_out: Vec<Stmt> = Vec::new();

    for stmt in stmts {
        match stmt {
            ast::Stmt::Let(id, exp) => {
                map.insert(id.clone(), Exp::elab(exp, arena)?);
            }

            _ => stmts_out.push(Stmt::elab(stmt, arena)?),
        }
    }

    Ok((map, stmts_out))
}

impl<'a> Elaborate<'a, ast::Stmt> for Stmt<'a> {
    fn elab(f: &'a ast::Stmt, arena: &'a Arena) -> Result<Self, ElabError> {
        Ok(match f {
            // let is handled by scope
            ast::Stmt::Let(_, _) => unreachable!(),

            ast::Stmt::Scope(stmts) => {
                // first, collect all the binds and partially evaluate them
                let (binds, remaining_stmts) = separate_scope(stmts, arena)?;

                // for each bind: add it to our list of binds.
                // insert it into the highest spot in the list which does

                Stmt::Scope(recursive_bind(binds, &remaining_stmts))
            }

            ast::Stmt::Exp(exp) => Stmt::Exp(Exp::elab(exp, arena)?),

            ast::Stmt::If(exp, stmt) => {
                Stmt::If(Exp::elab(exp, arena)?, Arc::new(Stmt::elab(stmt, arena)?))
            }
            ast::Stmt::Match(matcher) => Stmt::Match(Matcher::elab(matcher, arena)?),
        })
    }
}

impl<'a> Elaborate<'a, ast::Exp> for Exp<'a> {
    fn elab(f: &'a ast::Exp, arena: &'a Arena) -> Result<Self, ElabError> {
        Ok(match f {
            ast::Exp::BoolLit(b) => Exp::Static(arena.alloc(Val::Bool(*b))),
            ast::Exp::NumLit(n) => Exp::Static(arena.alloc(Val::Num(*n))),
            ast::Exp::SeqLit(s) => Exp::Static(arena.alloc(Val::Seq(s))),
            ast::Exp::StrLit(s) => Exp::StrLit(
                s.iter()
                    .map(|reg| match reg {
                        ast::StrLitReg::Exp(e) => Exp::elab(e, arena).map(StrLitReg::Exp),
                        ast::StrLitReg::Str(s) => Ok(StrLitReg::Str(s)),
                    })
                    .collect::<Result<Vec<_>, _>>()?,
            ),
            ast::Exp::HandlerLit(h) => Exp::Static(arena.alloc(Val::Handler(h.clone()))),

            ast::Exp::ListLit(l) => Exp::ListLit(
                l.iter()
                    .map(|e0| Exp::elab(e0, arena))
                    .collect::<Result<Vec<_>, _>>()?,
            ),

            ast::Exp::StructLit(fields) => Exp::StructLit(
                fields
                    .iter()
                    .map(|(id, exp)| Exp::elab(exp, arena).map(|e| (&id[..], e)))
                    .collect::<Result<HashMap<_, _>, _>>()?,
            ),

            ast::Exp::Add(exp, id) => Exp::Add(Arc::new(Exp::elab(exp, arena)?), id),
            ast::Exp::Id(id) => Exp::Id(id),
            ast::Exp::Call(fn_id, args) => Exp::Call(
                fn_id,
                args.iter()
                    .map(|e0| Exp::elab(e0, arena))
                    .collect::<Result<Vec<_>, _>>()?,
            ),
        })
    }
}
