use super::{Exp, Id};

struct IdCollector {}

pub fn ids<'a>(exp: &Exp<'a>) -> Vec<&'a Id> {
    ids_exp(exp)
}
pub fn ids_exp<'a>(exp: &Exp<'a>) -> Vec<&'a Id> {
    match exp {
        Exp::Static(_) => vec![],
        Exp::StrLit(s) => s
            .iter()
            .flat_map(|r| match r {
                super::StrLitReg::Exp(e) => ids(e),
                super::StrLitReg::Str(_) => vec![],
            })
            .collect(),
        Exp::ListLit(list) => list.iter().flat_map(|exp| ids(exp)).collect(),
        Exp::StructLit(fields) => fields.iter().flat_map(|(_, exp)| ids(exp)).collect(),
        Exp::Add(exp, _) => ids(exp),
        Exp::Id(id) => vec![*id],
        Exp::Call(_, args) => args.iter().flat_map(|exp| ids(exp)).collect(),
    }
}
