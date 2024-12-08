use super::*;

pub trait Subs<P> {
    fn subs(&self, id: &Id, val: &Exp) -> P;
}

impl<'a: 'b, 'b> Stmt<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> Stmt<'b> {
        match self {
            Stmt::Static(s) => Stmt::Static(s.clone()),
            Stmt::Scope(stmts) => Stmt::Scope(stmts.iter().map(|s| s.subs(id, val)).collect()),
            Stmt::Exp(exp) => Stmt::Exp(exp.subs(id, val)),
            Stmt::If(exp, stmt) => Stmt::If(exp.subs(id, val), Arc::new(stmt.subs(id, val))),
            Stmt::Match(matcher) => Stmt::Match(matcher.subs(id, val)),
        }
    }
}

impl<'a: 'b, 'b> Exp<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> Exp<'b> {
        match self {
            Exp::Static(s) => Exp::Static(s.clone()),
            Exp::StrLit(regs) => Exp::StrLit(
                regs.iter()
                    .map(|reg| match reg {
                        StrLitReg::Exp(exp) => StrLitReg::Exp(exp.subs(id, val)),
                        _ => reg.clone(),
                    })
                    .collect(),
            ),
            Exp::ListLit(exps) => Exp::ListLit(exps.iter().map(|exp| exp.subs(id, val)).collect()),
            Exp::StructLit(fields) => Exp::StructLit(
                fields
                    .iter()
                    .map(|(id0, exp)| (*id0, exp.subs(id, val)))
                    .collect(),
            ),

            Exp::Add(exp, id0) => Exp::Add(Arc::new(exp.subs(id, val)), *id0),
            Exp::Id(id0) => {
                if (*id0).eq(id) {
                    val.clone()
                } else {
                    self.clone()
                }
            }
            Exp::Call(fn_id, args) => {
                Exp::Call(*fn_id, args.iter().map(|arg| arg.subs(id, val)).collect())
            }
        }
    }
}
