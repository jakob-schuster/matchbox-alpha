use handler::Eff;
use util::{seq_to_string, Arena};

use crate::lang::ast::core::eval::{apply::apply, EvalError};

use super::*;

impl<'a> Matcher<'a> {
    pub fn eval(&self, arena: &'a Arena, ctx: &'a Context) -> Result<Vec<Eff>, EvalError<'a>> {
        // handle each arm,
        // and then if possible return a new scope of the arms
        match (self.read.eval(arena, ctx), self.seq.eval(arena, ctx)) {
            (Ok(new_read_val), Ok(new_seq_val)) => {
                // we have a read and a seq
                if let (Val::Struct(StructVal::Read(new_read)), Val::Seq(new_seq)) =
                    (new_read_val, new_seq_val)
                {
                    for par_arm in &self.arms {
                        let effs = match par_arm {
                            Par::Val(v) => v.eval(arena, ctx),
                            Par::Exp(e) => e.eval(&self.config, new_read, new_seq, arena, ctx),
                        }?;

                        if !effs.is_empty() {
                            // if possible, return early
                            // this would be different on "all" mode
                            return Ok(effs);
                        }
                    }

                    // if no arms matched,
                    // then no effects to return
                    Ok(vec![])
                } else {
                    Err(EvalError::TypeError(new_read_val.clone()))
                }
            }
            (Err(e1), _) => Err(e1),
            (_, Err(e2)) => Err(e2),
        }
    }

    pub fn simplify(&self, arena: &'a Arena, ctx: &'a Context) -> Par<Matcher<'a>, Vec<Eff>> {
        if let Ok(effs) = self.eval(arena, ctx) {
            return Par::Val(effs);
        }

        Par::Exp(Matcher::new(
            self.config.clone(),
            self.read.simplify(arena, ctx),
            self.seq.simplify(arena, ctx),
            self.arms
                .iter()
                .map(|a| {
                    a.map_cl(|a0| {
                        a0.simplify(
                            &self.config,
                            &self.read.simplify(arena, ctx),
                            &self.seq.simplify(arena, ctx),
                            arena,
                            ctx,
                        )
                        .unwrap()
                    })
                })
                .collect(),
        ))
    }
}

impl<'a, 'b> Arm<'a> {
    pub fn eval(
        &self,
        config: &Config,
        read: &'b Read<'a>,
        seq: &'b [u8],
        arena: &'a Arena,
        ctx: &'a Context,
    ) -> Result<Vec<Eff>, EvalError<'a>> {
        // try to bind everything
        let binds = self
            .binds
            .iter()
            .map(|(id, ev)| {
                let v = ev.eval(arena, ctx)?;
                match v {
                    Val::List(l) => Ok((*id, l)),
                    _ => Err(EvalError::TypeError(v.clone())),
                }
            })
            .collect::<Result<HashMap<_, _>, _>>()?;

        // make sure all the binds are ready
        // first, evaluate the ops
        let ops_new: Vec<&Op> = self
            .ops
            .iter()
            .map(|op| {
                Ok(match op {
                    Par::Val(o) => o,
                    Par::Exp(e) => arena.alloc(e.eval(config, &binds, arena, ctx)?),
                })
            })
            .try_collect()?;

        let locs: Vec<(Loc, Pos)> = vec![(0, 0), (self.end, seq.len() as Pos)];

        // this will result in a bunch of possible worlds;
        // each of these is made of binds;
        // each of these is substituted into the scope
        // resulting in a list
        let new_ctxs = ops_new.iter().fold(vec![(vec![], locs)], |ctxs, o| {
            ctxs.iter()
                .flat_map(|(bind_ctx, loc_ctx)| {
                    // todo: make this whole thing
                    // handle errors correctly
                    o.eval_ctx(read, seq, bind_ctx, loc_ctx, arena, ctx)
                        .unwrap()
                })
                .collect::<Vec<_>>()
        });

        let mut a = new_ctxs.iter().map(|(i, _)| {
            let mut new_ctx1 = ctx.clone();
            for (id, val) in i {
                new_ctx1.insert(id, (*val).clone());
            }
            new_ctx1
        });

        // this is for one:

        let mut effs = vec![];
        if let Some(first) = a.next() {
            let new_effs = self.stmt.eval(arena, &first).unwrap();

            for eff in new_effs {
                effs.push(eff);
            }
        }

        // for each in a {
        //     let new_effs = self.stmt.eval(arena, &each)
        //         .unwrap();

        //     for eff in new_effs {
        //         effs.push(eff);
        //     }
        // }

        // this is for all:
        // for s in stmts {
        //     let a = match s {
        //         Par::Val(v) => Ok(v),

        //         Par::Exp(s0) => s0.lazy_exec(arena),
        //     };

        //     match a {
        //         Ok(v) => for eff in v {
        //             effs.push(eff);
        //         },
        //         // todo: fix this
        //         Err(err) => Err(err).unwrap(),
        //     }
        // }

        Ok(effs)
    }
    pub fn simplify(
        &self,
        config: &Config,
        read: &'b Exp<'a>,
        seq: &'b Exp<'a>,
        arena: &'a Arena,
        ctx: &'a Context,
    ) -> Result<ParArm<'a>, EvalError<'a>> {
        // first try to eval
        match (read, seq) {
            (Exp::Static(read_val), Exp::Static(seq_val)) => match (read_val, seq_val) {
                (Val::Struct(StructVal::Read(read_raw)), Val::Seq(seq_raw)) => {
                    // must be able to simplify completely
                    Ok(Par::Val(Stmt::Static(
                        self.eval(config, read_raw, seq_raw, arena, ctx)?,
                    )))
                }
                _ => Err(EvalError::TypeError((*read_val).clone())),
            },
            _ => {
                // first, look at the binds
                let binds_new: HashMap<_, _> = self
                    .binds
                    .iter()
                    .map(|(id, e)| (*id, e.simplify(arena, ctx)))
                    .collect();

                let bound = binds_new
                    .iter()
                    // evaluate each one as much as possible
                    .flat_map(|(id, ev)| match ev {
                        Exp::Static(v) => match v {
                            Val::List(l) => Some((*id, l)),
                            _ => panic!("Not binding a List!"),
                        },
                        _ => None,
                    })
                    // .map(|(a,b)| (a.clone(), b.clone()))
                    .collect::<HashMap<_, _>>();

                // first, evaluate the ops
                let ops_new: Vec<_> = self
                    .ops
                    .iter()
                    .map(|op| {
                        op.try_map_cl(|o| match o.eval(config, &bound, arena, ctx) {
                            Ok(a) => Ok(Par::Val(a)),
                            Err(b) => Ok(Par::Exp(o.clone())),
                        })
                    })
                    .try_collect()?;

                let stmt_new = self.stmt.simplify(arena, ctx);

                Ok(Par::Exp(Arm::new(binds_new, ops_new, stmt_new, self.end)))
            }
        }
    }
}

impl<'a: 's, 's> Op<'a> {
    /// Executes the Op in the context,
    /// returning a new context of binds made by the Op.
    fn eval_ctx(
        &self,
        read: &'a Read<'a>,
        seq: &'a [u8],
        bind_ctx: &BindCtx<'a>,
        loc_ctx: &LocCtx,
        arena: &'a Arena,
        ctx: &'a Context,
    ) -> Result<Vec<(BindCtx<'a>, LocCtx)>, OpError> {
        match self {
            Op::Let(loc, loc_expr) => match loc_expr.eval(loc_ctx) {
                Ok(pos) => {
                    if pos >= 0 && pos <= seq.len() as Pos {
                        Ok(Vec::from([(
                            bind_ctx.clone(),
                            loc_ctx
                                .iter()
                                .chain([(*loc, pos)].iter())
                                .cloned()
                                .collect(),
                        )]))
                    } else {
                        Ok(Vec::from([]))
                    }
                }
                Err(err) => Err(OpError::LocError(err)),
            },
            Op::Restrict(binds, search_ran, fixed, save_rans) => {
                let filtered = binds
                    .iter()
                    // filter out any that would incur a contradictory binding
                    .filter(|(bs, _)| {
                        bs.iter().all(|(id, val)| {
                            if let Some(val2) = lookup_bind(bind_ctx, id) {
                                (*val).eq(val2)
                            } else {
                                true
                            }
                        })
                    });

                let mut accumulated = vec![];
                for (local_binds, exp_seq) in filtered {
                    let matches = exp_seq.find_all_disjoint(
                        seq,
                        &search_ran
                            .map(|i| lookup(loc_ctx, *i).expect("Bad search rans!") as usize),
                        fixed,
                    );

                    // if we have found the pattern sufficient times
                    if matches.len() >= save_rans.len() {
                        // // get the
                        // let min_edit_dist = matches.iter()
                        //     .map(|a| a.dist)
                        //     .sorted()
                        //     .take(save_rans.len())
                        //     .last();

                        let combs = matches
                            .iter()
                            .combinations(save_rans.len())
                            .map(|c| {
                                (
                                    bind_ctx.iter().chain(local_binds).cloned().collect_vec(),
                                    c.iter()
                                        .enumerate()
                                        .flat_map(|(i, mat)| {
                                            loc_ctx
                                                .iter()
                                                .chain(
                                                    [
                                                        (
                                                            save_rans
                                                                .get(i)
                                                                .expect("Bad save rans!")
                                                                .start,
                                                            mat.ran.start as i32,
                                                        ),
                                                        (
                                                            save_rans
                                                                .get(i)
                                                                .expect("Bad save rans!")
                                                                .end,
                                                            mat.ran.end as i32,
                                                        ),
                                                    ]
                                                    .iter(),
                                                )
                                                .cloned()
                                                .collect_vec()
                                        })
                                        .collect_vec(),
                                    c.iter().fold(0, |acc, mat| acc + mat.dist),
                                )
                            })
                            .collect_vec();

                        // return early - we've found it!
                        // return Ok(combs)

                        // return best

                        // return all matching patterns
                        for v in combs {
                            accumulated.push(v);
                        }
                    }
                }

                // for best

                if !accumulated.is_empty() {
                    let best_dist = accumulated
                        .clone()
                        .into_iter()
                        .map(|(_, _, dist)| dist)
                        .min()
                        .unwrap();
                    let final_accumulated_2 = accumulated
                        .into_iter()
                        .filter(|(_, _, dist)| *dist == best_dist)
                        .map(|(a, b, _)| (a, b))
                        .collect();

                    Ok(final_accumulated_2)
                } else {
                    Ok(vec![])
                }
            }

            Op::Name(id, ran) => match ran.map(|loc| lookup(loc_ctx, *loc)).to_tuple() {
                (Ok(start), Ok(end)) => {
                    if start <= end {
                        let mut new_bind_ctx = bind_ctx.clone();

                        match apply(
                            &Id::from("slice"),
                            &[
                                arena.alloc(Val::Struct(StructVal::Read(read))),
                                arena.alloc(Val::Num(*start)),
                                arena.alloc(Val::Num(*end)),
                            ],
                            arena,
                        ) {
                            Ok(slice) => {
                                new_bind_ctx.push((*id, slice));

                                Ok(Vec::from([(new_bind_ctx, loc_ctx.clone())]))
                            }
                            Err(e) => {
                                panic!("error {:?}", e);
                            }
                        }
                    } else {
                        println!("start {} end {}", start, end);
                        panic!()
                    }
                }
                _ => Err(OpError::LocError(LocError::LocNonExistent(ran.start))),
            },
        }
    }
}
