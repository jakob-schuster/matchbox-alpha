use crate::{
    lang::ast::{
        self,
        core::elab::{ElabError, Elaborate},
        MatchConfig,
    },
    util::{Arena, Ran},
};

use super::*;

impl<'a> Elaborate<'a, ast::Matcher> for Matcher<'a> {
    fn elab(f: &'a ast::Matcher, arena: &'a Arena) -> Result<Self, ast::core::elab::ElabError> {
        Ok(Matcher {
            config: Config::elab(&f.config, arena)?,
            read: Exp::elab(&f.read, arena)?,
            seq: Exp::Add(
                Arc::new(Exp::elab(&f.read, arena)?),
                arena.alloc(Id::from("seq")),
            ),
            arms: f
                .arms
                .iter()
                .map(|arm| Arm::elab(arm, arena).map(Par::Exp))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl<'a> Elaborate<'a, ast::Arm> for Arm<'a> {
    fn elab(f: &'a ast::Arm, arena: &'a Arena) -> Result<Self, ast::core::elab::ElabError> {
        let prearm = PreArm::elab(f, arena)?;

        Ok(prearm)
    }
}

impl<'a> Elaborate<'a, ast::MatchConfig> for Config {
    fn elab(f: &'a ast::MatchConfig, arena: &'a Arena) -> Result<Self, ElabError> {
        Ok(Config { error: f.error })
    }
}

#[derive(Clone, PartialEq, Debug)]
struct PreArm<'a> {
    binds: HashMap<&'a Id, Exp<'a>>,

    regs: Vec<(Reg<'a>, Ran<i32>)>,

    named: Vec<(&'a Id, Ran<i32>)>,
    sized: Vec<(Exp<'a>, Ran<i32>)>,

    stmt: Stmt<'a>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Reg<'a> {
    Hole,
    Exp(Vec<&'a Id>, Exp<'a>),
}

fn flatten_regs<'a>(
    binds: &[Id],
    i: Loc,
    regs: Vec<&'a ast::Reg>,
    sized_acc: Vec<(Exp<'a>, Ran<Loc>)>,
    named_acc: Vec<(&'a Id, Ran<Loc>)>,
    regs_acc: Vec<(Reg<'a>, Ran<Loc>)>,
    arena: &'a Arena,
) -> Result<
    (
        Vec<(Exp<'a>, Ran<Loc>)>,
        Vec<(&'a Id, Ran<Loc>)>,
        Vec<(Reg<'a>, Ran<Loc>)>,
    ),
    ElabError,
> {
    let first = regs.first();
    let rest = regs.iter().skip(1).cloned().collect_vec();

    match first {
        Some(reg) => match reg {
            ast::Reg::Hole => flatten_regs(
                binds,
                i + 1,
                rest,
                sized_acc,
                named_acc,
                regs_acc
                    .into_iter()
                    .chain([(Reg::Hole, Ran::new(i, i + 1))])
                    .collect(),
                arena,
            ),
            ast::Reg::Exp(e) => flatten_regs(
                binds,
                i + 1,
                rest,
                sized_acc,
                named_acc,
                regs_acc
                    .into_iter()
                    .chain([(
                        Reg::Exp(
                            // collect only the ids present in both the
                            // exp and the binds
                            visit::ids(&Exp::elab(e, arena)?)
                                .into_iter()
                                .filter(|id| binds.contains(id))
                                .collect(),
                            Exp::elab(e, arena)?,
                        ),
                        Ran::new(i, i + 1),
                    )])
                    .collect(),
                arena,
            ),
            ast::Reg::Named(id, inner_regs) => flatten_regs(
                binds,
                i,
                inner_regs.iter().chain(rest).collect_vec(),
                sized_acc,
                named_acc
                    .into_iter()
                    .chain([(id, Ran::new(i, i + inner_regs.len() as Loc))])
                    .collect(),
                regs_acc,
                arena,
            ),
            ast::Reg::Sized(exp, inner_regs) => flatten_regs(
                binds,
                i,
                inner_regs.iter().chain(rest).collect_vec(),
                sized_acc
                    .into_iter()
                    .chain([(
                        Exp::elab(exp, arena)?,
                        Ran::new(i, i + inner_regs.len() as Loc),
                    )])
                    .collect_vec(),
                named_acc,
                regs_acc,
                arena,
            ),
        },
        None => Ok((sized_acc, named_acc, regs_acc)),
    }
}

impl<'a> PreArm<'a> {
    fn elab(arm: &'a ast::Arm, arena: &'a Arena) -> Result<Arm<'a>, ElabError> {
        // flatten

        let (sized, named, regs) = flatten_regs(
            &arm.binds.iter().map(|(id, _)| id.clone()).collect_vec()[..],
            0,
            arm.regs.iter().collect(),
            vec![],
            vec![],
            vec![],
            arena,
        )?;

        // then handle the binds
        let binds = arm
            .binds
            .iter()
            .map(|(id, exp)| Exp::elab(exp, arena).map(|a| (id, a)))
            .collect::<Result<HashMap<_, _>, _>>()?;

        // the locations we know so far
        let mut known: Vec<Loc> = vec![0, regs.len() as Loc];

        let mut ops: Vec<ParOp> = vec![];

        fn check_all_fixed_lens<'a>(
            ops: &Vec<ParOp<'a>>,
            known: &mut Vec<Loc>,
            sized: &Vec<(Exp<'a>, Ran<i32>)>,
            arena: &'a Arena,
        ) -> (Vec<Loc>, Vec<ParOp<'a>>) {
            fn learn_new_fixed_lens<'aa>(
                known: &mut Vec<Loc>,
                fixed_lens: &[(Exp<'aa>, Ran<Loc>)],
                arena: &'aa Arena,
            ) -> Vec<ParOp<'aa>> {
                let mut ops = vec![];
                let mut new_known = known.clone();

                for (expr_num, ran) in fixed_lens {
                    match ran.map(|l| known.contains(l)).to_tuple() {
                        // todo: insert something that actually verifies the length?
                        (true, true) => {}

                        // if one end is known, insert a let operation
                        (true, false) => {
                            ops.push(Par::Exp(PreOp::Let(
                                ran.end,
                                PreLocExp::Offset(
                                    Arc::new(Par::Val(LocExp::Id(ran.start))),
                                    expr_num.clone(),
                                ),
                            )));
                            new_known.push(ran.end);
                        }

                        (false, true) => {
                            ops.push(Par::Exp(PreOp::Let(
                                ran.start,
                                PreLocExp::Offset(
                                    Arc::new(Par::Val(LocExp::Id(ran.end))),
                                    // call the negative function
                                    // (jesus christ what is this)
                                    Exp::Call(
                                        arena.alloc(Id::from("minus")),
                                        vec![expr_num.clone()],
                                    ),
                                ),
                            )));

                            new_known.push(ran.start);
                        }

                        (false, false) => {}
                    }
                }

                known.clone_from(&new_known);

                ops
            }

            // first check if we know one side of any fixed-length regions
            let mut ops = ops.clone();
            let mut known = known.clone();

            let mut ops_new = ops.clone();
            ops_new.extend(learn_new_fixed_lens(&mut known, sized, arena));
            while !ops.eq(&ops_new) {
                // update the old ops
                ops.clone_from(&ops_new);
                // extend the new ops
                ops_new.extend(learn_new_fixed_lens(&mut known, sized, arena));
            }
            ops.clone_from(&ops_new);

            (known, ops)
        }

        // first, the naive approach - just bind everything left to right

        fn get_tightest_known(known_locs: &[Loc], ran: &Ran<Loc>) -> Ran<Loc> {
            if let (Some(start), Some(end)) = (
                known_locs.iter().filter(|loc| **loc <= ran.start).max(),
                known_locs.iter().filter(|loc| **loc >= ran.end).min(),
            ) {
                Ran::new(*start, *end)
            } else {
                panic!("Didn't know the necessary Locs!");
            }
        }

        // rank the binds - with ones as-yet-unknown ranking the worst

        // rank the fixed-size regions we know,
        // with as-yet-unknown ranking the worst

        // in that order, can we make any restrictions within these regions?

        let bindable_regs = regs.iter().flat_map(|(reg, ran)| match reg {
            Reg::Hole => None,
            Reg::Exp(binds, exp) => Some((ran.clone(), (binds, exp.clone()))),
        });

        for (ran, (binds, exp)) in bindable_regs {
            // first, do the whole checking-for-range-restrictions thing
            (known, ops) = check_all_fixed_lens(&ops, &mut known, &sized, arena);

            let search = get_tightest_known(&known, &ran);

            // todo: check if the loc is fixed
            let fixed = Ran::new(search.start == ran.start, search.end == ran.end);

            ops.push(Par::Exp(PreOp::Restrict(
                binds.clone(),
                exp,
                search,
                fixed,
                vec![ran.clone()],
            )));

            known.push(ran.start);
            known.push(ran.end);
        }

        // check for range restriction one final time
        (_, ops) = check_all_fixed_lens(&ops, &mut known, &sized, arena);

        // then add all the names!
        for (id, ran) in named {
            ops.push(Par::Val(Op::Name(id, ran.clone())));
        }

        Ok(Arm::new(
            binds,
            ops,
            Stmt::elab(&arm.stmt, arena)?,
            regs.len() as Loc,
        ))
    }
}

// #[cfg(test)]
// mod test {
//     use std::collections::HashMap;

//     use crate::{lang::ast::core::{matcher::{elab::{PreArm, Reg}, Arm, LocExp, Op, PreLocExp, PreOp}, Exp, Id, Par, Stmt, Val}, util::Ran};

//     #[test]
//     fn comp_pat1() {
//         let p = PreArm {
//             binds: HashMap::from([

//             ]),

//             regs: vec![
//                 (Reg::Hole, Ran::new(0, 1)),
//                 (Reg::Hole, Ran::new(0, 1)),
//             ],

//             named: vec![
//                 (Id::from("first"), Ran::new(0, 1))
//             ],
//             sized: vec![
//                 (Par::Val(Val::Num(5)), Ran::new(0, 1))
//             ],
//             stmt: Par::Exp(Stmt::Exp(
//                 Par::Exp(Exp::Call(
//                     Id::from("out"),
//                     vec![
//                         Par::Exp(Exp::Id(Id::from("first"))),
//                         Par::Val(Val::Num(1))
//                     ]
//                 ))
//             )),
//         };

//         let v = Arm {
//             binds: HashMap::new(),
//             ops: vec![
//                 Par::Exp(PreOp::Let(1, PreLocExp::Offset(Arc::new(Par::Val(LocExp::Id(0))), Par::Val(Val::Num(5))))),
//                 Par::Val(Op::Name(Id::from("first"), Ran { start: 0, end: 1 }))
//             ],
//             stmt: Par::Exp(Stmt::Exp(
//                 Par::Exp(Exp::Call(
//                     Id::from("out"),
//                     vec![
//                         Par::Exp(Exp::Id(Id::from("first"))),
//                         Par::Val(Val::Num(1))
//                     ]
//                 ))
//             )),
//             end: 2,
//         };

//         assert_eq!(p.comp(), v);
//     }

//     #[test]
//     pub fn comp_pat2() {
//         let p = PreArm {
//             binds: HashMap::from([
//                 (Id::from("b"), Par::Val(Val::List(vec![
//                     Val::Num(1),
//                     Val::Num(2),
//                 ])))
//             ]),

//             regs: vec![
//                 (Reg::Hole, Ran::new(0, 1)),
//                 (Reg::Hole, Ran::new(1, 2)),
//                 (Reg::Exp(vec![Id::from("b")], Par::Exp(Exp::Id(Id::from("b")))), Ran::new(2, 3)),
//                 (Reg::Hole, Ran::new(3, 4)),
//                 (Reg::Exp(vec![], Par::Val(Val::Seq(Vec::from(b"AGCTAGTG")))), Ran::new(4, 5)),
//                 (Reg::Hole, Ran::new(5, 6))
//             ],

//             named: vec![
//                 (Id::from("first"), Ran::new(0, 1))
//             ],
//             sized: vec![
//                 (Par::Val(Val::Num(5)), Ran::new(0, 1))
//             ],
//             stmt: Par::Exp(Stmt::Exp(
//                 Par::Exp(Exp::Call(Id::from("out"), vec![
//                     Par::Exp(Exp::Id(Id::from("first"))),
//                     Par::Val(Val::Num(1))
//                 ]))
//             )),
//         };

//         let v = Arm {
//             binds: HashMap::from([(
//                 Id::from("b"),
//                 Par::Val(Val::List(vec![Val::Num(1), Val::Num(2)]))
//             )]),
//             ops: vec![
//                 Par::Exp(PreOp::Let(
//                     1,
//                     PreLocExp::Offset(
//                         Arc::new(Par::Val(LocExp::Id(0))),
//                         Par::Val(Val::Num(5))
//                     )
//                 )),
//                 Par::Exp(PreOp::Restrict(
//                     vec![Id::from("b")],
//                     Par::Exp(Exp::Id(Id::from("b"))),
//                     Ran { start: 1, end: 6 },
//                     Ran::new(false, false),
//                     vec![Ran { start: 2, end: 3 }]
//                 )),
//                 Par::Exp(PreOp::Restrict(
//                     vec![],
//                     Par::Val(Val::Seq(vec![65, 71, 67, 84, 65, 71, 84, 71])),
//                     Ran { start: 3, end: 6 },
//                     Ran::new(false, false),
//                     vec![Ran { start: 4, end: 5 }]
//                 )),
//                 Par::Val(Op::Name(
//                     Id::from("first"),
//                     Ran { start: 0, end: 1 })
//                 )],
//             stmt: Par::Exp(Stmt::Exp(
//                 Par::Exp(Exp::Call(
//                     Id::from("out"),
//                     vec![
//                         Par::Exp(Exp::Id(Id::from("first"))),
//                         Par::Val(Val::Num(1))
//                     ]
//                 ))
//             )),
//             end: 6
//         };

//         assert_eq!(p.comp(), v);
//     }

//     use super::*;

//     #[test]
//     fn prearm_test() {
//         println!("{:?}", flatten_regs(
//             &[],
//             0,
//             &[
//                 ast::Reg::Hole
//             ],
//             vec![],
//             vec![],
//             vec![]
//         ));
//     }
// }
