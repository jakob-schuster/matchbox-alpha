use std::collections::HashMap;

use crate::{
    myers::VarMyers,
    util::{self, Ran},
};

use super::{eval::EvalError, *};

mod elab;
mod eval;
mod subs;

#[derive(Debug, Clone, PartialEq)]
pub struct Matcher<'a> {
    config: Config,
    read: Exp<'a>,
    seq: Exp<'a>,
    arms: Vec<ParArm<'a>>,
}

impl<'a> Matcher<'a> {
    fn new(config: Config, read: Exp<'a>, seq: Exp<'a>, arms: Vec<ParArm<'a>>) -> Matcher<'a> {
        Matcher {
            config,
            read,
            seq,
            arms,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Config {
    error: f32,
}

type ParArm<'a> = Par<Arm<'a>, Stmt<'a>>;
#[derive(Debug, Clone, PartialEq)]
struct Arm<'a> {
    binds: HashMap<&'a Id, Exp<'a>>,
    ops: Vec<ParOp<'a>>,
    stmt: Stmt<'a>,
    end: Loc,
}
impl<'a> Arm<'a> {
    fn new(
        binds: HashMap<&'a Id, Exp<'a>>,
        ops: Vec<ParOp<'a>>,
        stmt: Stmt<'a>,
        end: Loc,
    ) -> Arm<'a> {
        Arm {
            binds,
            ops,
            stmt,
            end,
        }
    }
}

type ParOp<'a> = Par<PreOp<'a>, Op<'a>>;
#[derive(Debug, Clone, Eq, PartialEq)]
enum PreOp<'a> {
    Let(Loc, PreLocExp<'a>),
    Restrict {
        ids: Vec<&'a Id>,
        exp: Exp<'a>,
        search: Ran<Loc>,
        fixed: Ran<bool>,
        save: Vec<Ran<Loc>>,
    },
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Op<'a> {
    Let(Loc, LocExp),
    Restrict(Vec<(BindCtx<'a>, Seq)>, Ran<Loc>, Ran<bool>, Vec<Ran<Loc>>),
    Name(&'a Id, Ran<Loc>),
}

#[derive(Debug)]
enum LocError {
    LocNonExistent(Loc),
}

#[derive(Debug)]
enum OpError {
    LocError(LocError),
    Misc,
}

impl<'a: 'b, 'b> PreOp<'a> {
    fn eval(
        &self,
        config: &Config,
        bound: &HashMap<&Id, &Vec<&'a Val>>,
        arena: &'a Arena,
        ctx: &'a Context,
    ) -> Result<Op<'a>, EvalError<'a>> {
        match self {
            PreOp::Let(l, le) => Ok(Op::Let(*l, le.eval(arena, ctx)?)),
            PreOp::Restrict {
                ids,
                exp,
                search,
                fixed,
                save,
            } => {
                if ids.len().eq(&0) {
                    // special case - just wrap up the one value
                    let val = exp.eval(arena, ctx)?;

                    match val {
                        Val::Seq(bytes) => Ok(Op::Restrict(
                            vec![(vec![], Seq::new(bytes, config.error))],
                            search.clone(),
                            fixed.clone(),
                            save.clone(),
                        )),
                        _ => Err(EvalError::TypeError(val.clone())),
                    }
                } else {
                    ids.iter()
                        .map(|id| {
                            bound
                                .get(*id)
                                .map(|seqs| seqs.iter().map(|seq| (*id, *seq)).collect::<Vec<_>>())
                        })
                        .collect::<Option<Vec<Vec<(&Id, &Val)>>>>()
                        .map_or(
                            // not all binds are bound
                            Err(EvalError::PreOpError),
                            // all binds are bound
                            |binds| {
                                Ok(Op::Restrict(
                                    match &binds[..] {
                                        [first, rest @ ..] => first
                                            .iter()
                                            .flat_map(|first_val: &(&String, &Val)| {
                                                rest.iter().fold(
                                                    vec![vec![*first_val]],
                                                    |v: Vec<Vec<(&Id, &Val)>>, bind| {
                                                        v.iter()
                                                            .cartesian_product(bind)
                                                            .map(|(v, new)| {
                                                                v.iter()
                                                                    .chain([new])
                                                                    .cloned()
                                                                    .collect_vec()
                                                            })
                                                            .collect_vec()
                                                    },
                                                )
                                            })
                                            .collect(),

                                        // this can't happen (i think)
                                        [] => vec![],
                                    }
                                    .iter()
                                    .map(|bind_ctx| {
                                        let val = bind_ctx
                                            .iter()
                                            .fold(exp.clone(), |ev, (id, val)| {
                                                ev.subs(id, &Exp::Static(val))
                                            })
                                            .eval(arena, ctx)?;

                                        if let Val::Seq(s) = val {
                                            Ok((
                                                Vec::from_iter(bind_ctx.clone()),
                                                Seq::new(s, config.error),
                                            ))
                                        } else {
                                            panic!("Type error!")
                                        }
                                    })
                                    .try_collect()?,
                                    search.clone(),
                                    fixed.clone(),
                                    save.clone(),
                                ))
                            },
                        )
                }
            }
        }
    }
}

type ParLocExp<'a> = Par<PreLocExp<'a>, LocExp>;
#[derive(Debug, Clone, Eq, PartialEq)]
enum PreLocExp<'a> {
    Offset(Arc<ParLocExp<'a>>, Exp<'a>),
}

impl<'a> PreLocExp<'a> {
    fn eval(&self, arena: &'a Arena, ctx: &'a Context) -> Result<LocExp, EvalError<'a>> {
        match self {
            PreLocExp::Offset(le, e) => {
                let l = le.try_map_cl(|le0| le0.simplify(arena, ctx))?;
                let v = e.eval(arena, ctx)?;

                match &l {
                    Par::Val(l) => match v {
                        Val::Num(n) => Ok(LocExp::Offset(Arc::new(l.clone()), *n)),
                        _ => panic!("Bad type!"),
                    },

                    _ => Err(EvalError::PreOpError),
                }
            }
        }
    }

    fn simplify(&self, arena: &'a Arena, ctx: &'a Context) -> Result<ParLocExp<'a>, EvalError<'a>> {
        match self {
            PreLocExp::Offset(le, e) => {
                let l = le.try_map_cl(|le0| le0.simplify(arena, ctx))?;
                let e = e.simplify(arena, ctx);

                match (&l, &e) {
                    (Par::Val(l), Exp::Static(v)) => match v {
                        Val::Num(n) => Ok(Par::Val(LocExp::Offset(Arc::new(l.clone()), *n))),
                        _ => panic!("Bad type!"),
                    },
                    _ => Ok(Par::Exp(PreLocExp::Offset(Arc::new(l), e))),
                }
            }
        }
    }
}

// Awaiting concretization in a loc_ctx
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LocExp {
    Id(Loc),
    Offset(Arc<LocExp>, i32),
}

impl LocExp {
    /// Evaluate a loc to a pos in a given context
    fn eval(&self, loc_ctx: &LocCtx) -> Result<Pos, LocError> {
        match self {
            LocExp::Id(loc) => loc_ctx
                .iter()
                .find(|(loc2, _)| loc2.eq(loc))
                .map_or(Err(LocError::LocNonExistent(*loc)), |(_, pos)| Ok(*pos)),
            LocExp::Offset(loc_expr, offset) => loc_expr.eval(loc_ctx).map(|pos| pos + offset),
        }
    }
}

type Loc = i32;
type Pos = i32;

type BindCtx<'a> = Vec<(&'a Id, &'a Val<'a>)>;
fn lookup_bind<'a>(bind_ctx: &'a BindCtx, id: &Id) -> Option<&'a Val<'a>> {
    bind_ctx
        .iter()
        .rfind(|(bind2, _)| (*bind2).eq(id))
        .map(|(_, val)| *val)
}

type LocCtx = Vec<(Loc, Pos)>;
fn lookup(loc_ctx: &LocCtx, loc: Loc) -> Result<Pos, LocError> {
    loc_ctx
        .iter()
        .rfind(|(loc2, _)| loc2.eq(&loc))
        .map_or(Err(LocError::LocNonExistent(loc)), |(_, pos)| Ok(*pos))
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Seq {
    bytes: Vec<u8>,
    myers: VarMyers,
    dist: u8,
}

impl Seq {
    pub fn new(bytes: &[u8], error: f32) -> Seq {
        Seq {
            bytes: bytes.to_ascii_uppercase(),
            myers: VarMyers::new(&bytes.to_ascii_uppercase()),
            dist: (bytes.len() as f32 * error).floor() as u8,
        }
    }

    fn find_all_disjoint(
        &self,
        seq: &[u8],
        search_ran: &Ran<usize>,
        fixed: &Ran<bool>,
    ) -> Vec<Mat> {
        let (start, end): (i32, i32) = match (fixed.start, fixed.end) {
            // somehow pin both ends
            // - i guess just calculate straight levenshtein distance?
            (true, true) => todo!(),

            // pin the start
            (true, false) => (
                search_ran.start as i32,
                (search_ran.start + self.bytes.len() + self.dist as usize) as i32,
            ),

            // pin the end
            (false, true) => (
                search_ran.end as i32 - self.bytes.len() as i32 - self.dist as i32,
                search_ran.end as i32,
            ),

            // just grab the whole slice of the sequence
            (false, false) => (search_ran.start as i32, search_ran.end as i32),
        };

        // return early if the range is inappropriate
        if start < 0 || end > seq.len() as i32 {
            return vec![];
        }

        let trimmed = &seq[start as usize..end as usize];

        self.myers
            .find_all_disjoint(trimmed, self.dist)
            .iter()
            .map(|(mat_start, mat_end, dist)| {
                Mat::new(
                    Ran::new(*mat_start + start as usize, *mat_end + start as usize),
                    *dist as u8,
                )
            })
            .collect()
    }
}

#[derive(Debug)]
struct Mat {
    ran: Ran<usize>,
    dist: u8,
}

impl Mat {
    fn new(ran: Ran<usize>, dist: u8) -> Mat {
        Mat { ran, dist }
    }
}
