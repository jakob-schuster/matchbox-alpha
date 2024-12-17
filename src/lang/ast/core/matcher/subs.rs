use super::*;

impl<'a: 'b, 'b> Matcher<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> Matcher<'b> {
        Matcher {
            config: self.config.clone(),
            read: self.read.subs(id, val),
            seq: self.seq.subs(id, val),
            arms: self
                .arms
                .iter()
                .map(|a| a.map_cl(|a| a.subs(id, val)))
                .collect(),
        }
    }
}

impl<'a: 'b, 'b> Arm<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> ParArm<'b> {
        Par::Exp(Arm {
            binds: self
                .binds
                .iter()
                .map(|(id0, e)| (*id0, e.subs(id, val)))
                .collect(),
            ops: self
                .ops
                .iter()
                .map(|o| o.exp_map_cl(|a| a.subs(id, val)))
                .collect(),
            stmt: self.stmt.subs(id, val),
            end: self.end,
        })
    }
}

impl<'a: 'b, 'b> PreOp<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> PreOp<'b> {
        match self {
            PreOp::Let(a, b) => PreOp::Let(*a, b.subs(id, val)),
            PreOp::Restrict {
                ids: a,
                exp: b,
                search: c,
                fixed: d,
                save: e,
            } => PreOp::Restrict {
                ids: a.clone(),
                exp: b.subs(id, val),
                search: c.clone(),
                fixed: d.clone(),
                save: e.clone(),
            },
        }
    }
}

impl<'a: 'b, 'b> PreLocExp<'a> {
    pub fn subs(&self, id: &Id, val: &Exp<'b>) -> PreLocExp<'b> {
        match self {
            PreLocExp::Offset(l, le) => PreLocExp::Offset(
                Arc::new(l.exp_map_cl(|l| l.subs(id, val))),
                le.subs(id, val),
            ),
            _ => self.clone(),
        }
    }
}
