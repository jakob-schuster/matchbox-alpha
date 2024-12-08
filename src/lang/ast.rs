use core::handler::Handler;
use std::{collections::HashMap, rc::Rc};

use super::cli::GlobalConfig;

pub mod core;
pub mod parse;
pub mod read;

#[derive(Clone, PartialEq, Debug)]
pub struct Prog {
    stmt: Stmt,
}

#[derive(Clone, PartialEq, Debug)]
enum Stmt {
    Let(Id, Exp),
    Scope(Vec<Stmt>),
    Exp(Exp),
    If(Exp, Rc<Stmt>),
    Match(Matcher),
}

#[derive(Clone, PartialEq, Debug)]
struct Matcher {
    config: MatchConfig,
    read: Exp,
    arms: Vec<Arm>,
}
impl Matcher {
    fn new_default(arm: Arm, global_config: &GlobalConfig) -> Matcher {
        Matcher {
            config: MatchConfig::new(global_config.error),
            read: Exp::Id(Id::from("read")),
            arms: vec![arm],
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct MatchConfig {
    error: f32,
}

impl MatchConfig {
    fn new(error: f32) -> Self {
        Self { error }
    }
}

#[derive(Clone, PartialEq, Debug)]
struct Arm {
    binds: Vec<(Id, Exp)>,
    regs: Vec<Reg>,
    stmt: Stmt,
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Reg {
    Hole,
    Exp(Exp),

    Named(Id, Vec<Reg>),
    Sized(Exp, Vec<Reg>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Exp {
    BoolLit(bool),
    NumLit(i32),
    SeqLit(Vec<u8>),
    StrLit(Vec<StrLitReg>),
    HandlerLit(Handler),

    ListLit(Vec<Exp>),
    StructLit(HashMap<Id, Exp>),

    Add(Rc<Exp>, Id),
    Id(Id),

    Call(Id, Vec<Exp>),
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum StrLitReg {
    Exp(Exp),
    Str(String),
}

type Id = String;
