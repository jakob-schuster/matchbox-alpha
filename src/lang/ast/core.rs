use std::{collections::HashMap, rc::Rc, sync::Arc};

use crate::util::{Arena, Par, Ran};
use eval::EvalError;
use itertools::Itertools;

use super::read::Read;

pub mod elab;
pub mod eval;
pub mod handler;
mod matcher;
mod subs;
pub mod visit;

pub type Context<'a> = HashMap<&'a Id, Val<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct Prog<'a> {
    stmt: Stmt<'a>,
}

#[derive(Debug, Clone, PartialEq)]
enum Stmt<'a> {
    Static(Vec<handler::Eff>),

    Scope(Vec<Stmt<'a>>),
    Exp(Exp<'a>),
    If(Exp<'a>, Arc<Stmt<'a>>),
    Match(matcher::Matcher<'a>),
}

type Id = String;

#[derive(Debug, Clone, Eq, PartialEq)]
enum Exp<'a> {
    Static(&'a Val<'a>),

    StrLit(Vec<StrLitReg<'a>>),
    ListLit(Vec<Exp<'a>>),
    StructLit(HashMap<&'a str, Exp<'a>>),

    Add(Arc<Exp<'a>>, &'a Id),
    Id(&'a Id),

    Call(&'a Id, Vec<Exp<'a>>),
}
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Val<'a> {
    Bool(bool),
    Num(i32),
    Str(&'a str),
    Seq(&'a [u8]),

    Handler(handler::Handler),
    Eff(&'a Val<'a>, handler::Handler),

    List(Vec<&'a Val<'a>>),
    Struct(StructVal<'a>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StructVal<'a> {
    Regular { map: HashMap<&'a str, &'a Val<'a>> },
    Read(&'a Read<'a>),
}

impl<'a: 'i, 'i> StructVal<'a> {
    pub fn address(&self, id: &'i str, arena: &'a Arena) -> Result<&'a Val<'a>, EvalError<'i>> {
        match self {
            StructVal::Regular { map } => match map.get(id) {
                Some(v) => Ok(v),
                None => Err(EvalError::IndexingInvalidStructField(id)),
            },
            StructVal::Read(read) => read.address(id, arena),
        }
    }

    pub fn with_all(
        &self,
        entries: &[(&'a str, &'a Val<'a>)],
        arena: &'a Arena,
    ) -> Result<StructVal<'a>, EvalError<'i>> {
        match self {
            StructVal::Regular { map } => {
                let mut new_map = map.clone();

                // add all the entries
                for (id, val) in entries {
                    new_map.insert(id, val);
                }

                Ok(StructVal::Regular { map: new_map })
            }

            // we have to convert to regular structval here..
            // no obvious cheaper option.
            StructVal::Read(r) => StructVal::Regular {
                map: r.to_regular_structval(arena)?,
            }
            .with_all(entries, arena),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum OwnedVal {
    Bool(bool),
    Num(i32),
    Str(String),
    Seq(Vec<u8>),

    Handler(handler::Handler),
    Eff(Arc<OwnedVal>, handler::Handler),

    List(Vec<OwnedVal>),
    Struct(HashMap<Id, Arc<OwnedVal>>),

    Read(OwnedRead),
}

#[derive(Debug, Clone, Eq, PartialEq)]
// naive implementation
pub enum OwnedRead {
    Seq {
        seq: Vec<u8>,
    },
    Fasta {
        id: String,
        desc: String,
        seq: Vec<u8>,
    },
    Fastq {
        id: String,
        desc: String,
        seq: Vec<u8>,
        qual: String,
    },
    Sam {
        qname: String,
        flag: u16,
        rname: String,
        pos: i64,
        mapq: u8,
        cigar: String,
        rnext: String,
        pnext: i64,
        tlen: i32,
        seq: Vec<u8>,
        qual: String,
    },
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum StrLitReg<'a> {
    Exp(Exp<'a>),
    Str(&'a str),
}
