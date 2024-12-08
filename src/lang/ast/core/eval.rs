use super::{Read, *};
use crate::util::{self, seq_to_string, Arena};
use apply::{apply, FunctionError};
use bio::io::fastq;
use core::str;
use ratatui::text::ToSpan;
use std::{
    fmt::{Display, Pointer},
    path::Path,
};

pub mod apply;

#[derive(Debug, Clone)]
pub enum EvalError<'a> {
    UnknownIdCalled(&'a Id),
    FunctionError(FunctionError<'a>),
    IndexingInvalidStructField(&'a str),
    TypeError(Val<'a>),
    UnresolvedExp(Exp<'a>),
    PreOpError,
    ReadFieldError,
}

impl<'a> Prog<'a> {
    pub fn eval(
        &self,
        read: &'a crate::lang::read::Read,
        arena: &'a Arena,
    ) -> Result<Vec<handler::Eff>, EvalError<'a>> {
        // first, substitute in the read
        let read_id = &Id::from("read");
        let read_val = read.to_val();

        let ctx = HashMap::from([(read_id, read_val)]);
        // then, execute the statement
        let v = self.stmt.eval(arena, &ctx).unwrap();

        // then, check the result
        Ok(v)
    }

    pub fn simplify(&self, arena: &'a Arena, ctx: &'a Context) -> Result<Prog<'a>, EvalError<'a>> {
        Ok(Prog {
            stmt: self.stmt.simplify(arena, ctx),
        })
    }
}

impl<'a> Stmt<'a> {
    pub fn eval(
        &self,
        arena: &'a Arena,
        ctx: &'a Context,
    ) -> Result<Vec<handler::Eff>, EvalError<'a>> {
        match self {
            Stmt::Static(effs) => Ok(effs.clone()),

            Stmt::Scope(stmts) => {
                let mut effs = vec![];
                for stmt in stmts {
                    let new_effs = stmt.eval(arena, ctx)?;

                    for eff in new_effs {
                        effs.push(eff);
                    }
                }

                // accumulated all the effects
                Ok(effs)
            }
            Stmt::Exp(exp) => match exp.eval(arena, ctx)? {
                Val::Eff(v, h) => Ok(vec![handler::Eff::new(
                    Arc::new(OwnedVal::from((*v).clone())),
                    h.clone(),
                )]),
                // return to stdout by default!
                v => Ok(vec![handler::Eff::new(
                    Arc::new(OwnedVal::from(v.clone())),
                    handler::Handler::Stdout,
                )]),
            },
            Stmt::If(e1, s) => match e1.eval(arena, ctx)? {
                Val::Bool(b) => {
                    if *b {
                        s.eval(arena, ctx)
                    } else {
                        Ok(vec![])
                    }
                }
                val => Err(EvalError::TypeError(val.clone())),
            },
            Stmt::Match(m) => m.eval(arena, ctx),
        }
    }

    pub fn simplify(&self, arena: &'a Arena, ctx: &'a Context) -> Stmt<'a> {
        // if we can evaluate it already, it's an early return
        if let Ok(val) = self.eval(arena, ctx) {
            return Stmt::Static(val);
        }

        // otherwise, just simplify all the subexps
        match self {
            Stmt::Static(s) => Stmt::Static(s.clone()),
            Stmt::Scope(s) => Stmt::Scope(s.iter().map(|s0| s0.simplify(arena, ctx)).collect()),
            Stmt::Exp(e) => Stmt::Exp(e.simplify(arena, ctx)),
            Stmt::If(e, s) => Stmt::If(e.simplify(arena, ctx), Arc::new(s.simplify(arena, ctx))),
            Stmt::Match(m) => match m.simplify(arena, ctx) {
                Par::Val(v) => Stmt::Static(v),
                Par::Exp(e) => Stmt::Match(e),
            },
        }
    }
}

impl<'a: 'b, 'b> Exp<'a> {
    pub fn eval(
        &self,
        arena: &'a Arena,
        ctx: &'a Context<'a>,
    ) -> Result<&'a Val<'a>, EvalError<'a>> {
        match self {
            // todo: make this free
            Exp::Static(s) => Ok(s),
            Exp::StrLit(s) => {
                let mut new_s = vec![];
                for r in s {
                    new_s.push(match r {
                        StrLitReg::Exp(e) => match e.eval(arena, ctx)? {
                            Val::Str(st) => Ok(st),
                            v => Err(EvalError::TypeError(v.clone())),
                        },
                        StrLitReg::Str(st) => Ok(st),
                    }?);
                }

                Ok(arena.alloc(Val::Str(arena.alloc(new_s.iter().join("")))))
            }
            Exp::ListLit(l) => {
                let mut v = vec![];
                for e in l {
                    v.push(e.eval(arena, ctx)?);
                }
                Ok(arena.alloc(Val::List(v)))
            }
            Exp::StructLit(fs) => {
                let mut new_fs = HashMap::new();
                for (id, exp) in fs {
                    new_fs.insert(*id, exp.eval(arena, ctx)?);
                }
                Ok(arena.alloc(Val::Struct(StructVal::Regular { map: new_fs })))
            }
            Exp::Add(e, id) => match e.eval(arena, ctx)? {
                Val::Struct(s) => s.address(id, arena),

                // Val::Read(r) => r.address(id, &arena),
                v => Err(EvalError::TypeError(v.clone())),
            },
            Exp::Id(id) => match ctx.get(id) {
                Some(val) => Ok(val),
                None => Err(EvalError::UnknownIdCalled(id)),
            },
            Exp::Call(fn_id, args) => {
                let mut new_args = vec![];
                for arg in args {
                    new_args.push(arg.eval(arena, ctx)?);
                }

                Ok(apply(fn_id, &new_args[..], arena).map_err(EvalError::FunctionError)?)
            }
        }
    }

    pub fn simplify(&self, arena: &'a Arena, ctx: &'a Context) -> Exp<'a> {
        // if we can evaluate it already, it's an early return
        if let Ok(val) = self.eval(arena, ctx) {
            return Exp::Static(val);
        }

        // otherwise, just simplify all the subexps
        match self {
            Exp::Static(s) => Exp::Static(s.clone()),
            Exp::StrLit(r) => Exp::StrLit(
                r.iter()
                    .map(|r0| match r0 {
                        StrLitReg::Exp(e) => StrLitReg::Exp(e.simplify(arena, ctx)),
                        StrLitReg::Str(s) => StrLitReg::Str(s),
                    })
                    .collect(),
            ),
            Exp::ListLit(l) => Exp::ListLit(l.iter().map(|e| e.simplify(arena, ctx)).collect()),
            Exp::StructLit(fields) => Exp::StructLit(
                fields
                    .iter()
                    .map(|(a, b)| (*a, b.simplify(arena, ctx)))
                    .collect(),
            ),
            Exp::Add(e, id) => Exp::Add(Arc::new(e.simplify(arena, ctx)), id),
            Exp::Id(id) => Exp::Id(id),
            Exp::Call(fn_id, args) => Exp::Call(
                fn_id,
                args.iter().map(|arg| arg.simplify(arena, ctx)).collect(),
            ),
        }
    }
}

impl From<Val<'_>> for OwnedVal {
    fn from(value: Val) -> Self {
        match value {
            Val::Bool(b) => OwnedVal::Bool(b),
            Val::Num(n) => OwnedVal::Num(n),
            Val::Str(s) => OwnedVal::Str(String::from(s)),
            Val::Seq(s) => OwnedVal::Seq(Vec::from(s)),
            Val::Handler(h) => OwnedVal::Handler(h),
            Val::Eff(v, h) => OwnedVal::Eff(Arc::new(OwnedVal::from(v.clone())), h),
            Val::List(l) => {
                OwnedVal::List(l.iter().map(|v| OwnedVal::from((*v).clone())).collect())
            }
            Val::Struct(f) => match f {
                StructVal::Regular { map } => OwnedVal::Struct(
                    map.into_iter()
                        .map(|(id, val)| (id.to_string(), Arc::new(OwnedVal::from(val.clone()))))
                        .collect(),
                ),
                StructVal::Read(r) => OwnedVal::Read(OwnedRead::from(r)),
            }, // Val::Read(r) => OwnedVal::Read(OwnedRead::from(r)),
        }
    }
}

impl From<&Read<'_>> for OwnedRead {
    fn from(value: &Read<'_>) -> Self {
        match value {
            Read::Fasta(fasta_read) => OwnedRead::Fasta {
                id: fasta_read.id().to_string(),
                desc: fasta_read.desc().to_string(),
                seq: fasta_read.seq().to_vec(),
            },
            Read::Fastq(fastq_read) => OwnedRead::Fastq {
                id: fastq_read.id().to_string(),
                desc: fastq_read.desc().to_string(),
                seq: fastq_read.seq().to_vec(),
                qual: String::from_utf8(fastq_read.qual().to_vec()).unwrap(),
            },
            Read::Sam(sam_read) => OwnedRead::Sam {
                qname: str::from_utf8(sam_read.qname()).unwrap().to_string(),
                flag: sam_read.flag(),
                rname: str::from_utf8(sam_read.rname()).unwrap().to_string(),
                pos: sam_read.pos(),
                mapq: sam_read.mapq(),
                cigar: str::from_utf8(sam_read.cigar()).unwrap().to_string(),
                rnext: str::from_utf8(sam_read.rnext()).unwrap().to_string(),
                pnext: sam_read.pnext(),
                tlen: sam_read.tlen(),
                seq: sam_read.seq().to_vec(),
                qual: String::from_utf8(sam_read.qual().to_vec()).unwrap(),
            },
            Read::Seq(_) => todo!(),
            // Read::Seq { seq } =>
            //     OwnedRead::Seq { seq: seq.to_vec() },
            // Read::Fasta { name, seq } =>
            //     OwnedRead::Fasta { name: name.to_string(), seq: seq.to_vec() },
            // Read::Fastq { name, tags, seq, qual } =>
            //     OwnedRead::Fastq {
            //         name: name.to_string(),
            //         tags: tags.iter()
            //             .map(|tag| tag.to_string())
            //             .collect(),
            //         seq: seq.to_vec(),
            //         qual: qual.to_vec()
            //     },
        }
    }
}

impl Display for OwnedVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OwnedVal::Bool(b) => b.fmt(f),
            OwnedVal::Num(n) => n.fmt(f),
            // OwnedVal::Str(s) => format!("'{}'", s).fmt(f),
            OwnedVal::Str(s) => s.fmt(f),
            OwnedVal::Seq(s) => seq_to_string(s).fmt(f),
            OwnedVal::Handler(h) => h.fmt(f),
            OwnedVal::Eff(v, h) => format!("{} |> {}", v, h).fmt(f),
            OwnedVal::List(l) => format!("({})", l.into_iter().join(",")).fmt(f),
            OwnedVal::Struct(s) => format!(
                "{{{}}}",
                s.iter().map(|(a, b)| format!("{} : {}", a, b)).join(",")
            )
            .fmt(f),
            OwnedVal::Read(r) => r.fmt(f),
        }
    }
}
