use crate::token::*;

pub trait TDbg {
    fn indent(n: usize) {
        for _ in 0..n {
            print!("  |");
        }
    }
    fn p(n: usize, s: &str) {
        Self::indent(n);
        print!("{s}");
    }
    fn pln(n: usize, s: &str) {
        Self::indent(n);
        println!("{s}");
    }
    fn tdbg(&self, n: usize);
}
impl<T: TDbg> TDbg for Vec<T> {
    fn tdbg(&self, n: usize) {
        Self::pln(n, "List");
        if self.len() == 0 {
            Self::pln(n + 1, "empty")
        } else {
            for e in self {
                e.tdbg(n + 1);
            }
        }
    }
}

impl<T: TDbg> TDbg for Option<T> {
    fn tdbg(&self, n: usize) {
        match self {
            None => Self::pln(n, "None"),
            Some(v) => {
                Self::pln(n, "Some");
                v.tdbg(n + 1);
            }
        }
    }
}

impl TDbg for Toplevel {
    fn tdbg(&self, n: usize) {
        match self {
            Toplevel::Define(def) => {
                Self::pln(n, "Top(Def))");
                def.tdbg(n + 1);
            }
            Toplevel::Load(fname) => {
                Self::p(n, "Load");
                println!("[{fname}]");
            }
            Toplevel::Exp(exp) => {
                Self::pln(n, "Top(Exp)");
                exp.tdbg(n + 1);
            }
        }
    }
}
impl TDbg for Exp {
    fn tdbg(&self, n: usize) {
        match self {
            Exp::And(v) => {
                Self::pln(n, "Exp(And)");
                v.tdbg(n + 1);
            }
            Exp::Begin(v) => {
                Self::pln(n, "Exp(Begin)");
                v.tdbg(n + 1);
            }
            Exp::Or(v) => {
                Self::pln(n, "Exp(Or)");
                v.tdbg(n + 1);
            }
            Exp::Cond {
                branches,
                else_branch,
            } => {
                Self::pln(n, "Exp(Cond)");
                branches.tdbg(n + 1);
                else_branch.tdbg(n + 1);
            }
            Exp::Const(c) => {
                Self::pln(n, "Exp(Const)");
                c.tdbg(n + 1);
            }
            Exp::FunCall { fname, args } => {
                Self::pln(n, "Exp(FnCall)");
                fname.tdbg(n + 1);
                args.tdbg(n + 1);
            }
            Exp::Lambda(v, w) => {
                Self::pln(n, "Exp(Lambda)");
                v.tdbg(n + 1);
                w.tdbg(n + 1);
            }
            Exp::Id(v) => {
                Self::pln(n, "Exp(Id)");
                v.tdbg(n + 1);
            }
            Exp::Quote(v) => {
                Self::pln(n, "Exp(Quote)");
                v.tdbg(n + 1);
            }
            Exp::Set(v, w) => {
                Self::pln(n, "Exp(Set)");
                v.tdbg(n + 1);
                w.tdbg(n + 1);
            }
            Exp::Let { name, bind, body } => {
                Self::pln(n, "Exp(Let)");
                name.tdbg(n + 1);
                bind.tdbg(n + 1);
                body.tdbg(n + 1);
            }
            Exp::Let2(v, w) => {
                Self::pln(n, "Exp(Let2)");
                v.tdbg(n + 1);
                w.tdbg(n + 1);
            }
            Exp::LetRec(v, w) => {
                Self::pln(n, "Exp(LetRec)");
                v.tdbg(n + 1);
                w.tdbg(n + 1);
            }
            Exp::If {
                cond,
                then_exp,
                else_exp,
            } => {
                Self::pln(n, "Exp(If)");
                cond.tdbg(n + 1);
                then_exp.tdbg(n + 1);
                match else_exp {
                    None => Self::pln(n + 1, "None"),
                    Some(v) => {
                        Self::pln(n + 1, "Some");
                        v.tdbg(n + 1)
                    }
                }
            }
        }
    }
}
impl TDbg for Define {
    fn tdbg(&self, n: usize) {
        match self {
            Define::Func(id, prm, body) => {
                Self::pln(n, "DefFun");
                id.tdbg(n + 1);
                prm.tdbg(n + 1);
                body.tdbg(n + 1);
            }
            Define::Var(id, exp) => {
                Self::pln(n, "DefVar");
                id.tdbg(n + 1);
                exp.tdbg(n + 1)
            }
        }
    }
}
impl TDbg for Branch {
    fn tdbg(&self, n: usize) {
        match self {
            Branch { cond, then } => {
                Self::pln(n, "Branch");
                cond.tdbg(n + 1);
                then.tdbg(n + 1);
            }
        }
    }
}
impl TDbg for Bind {
    fn tdbg(&self, n: usize) {
        match self {
            Bind { name, val } => {
                Self::pln(n, "Bind");
                name.tdbg(n + 1);
                val.tdbg(n + 1);
            }
        }
    }
}
impl TDbg for Params {
    fn tdbg(&self, n: usize) {
        match &self {
            Params { prms, other } => {
                Self::pln(n, "Params");
                prms.tdbg(n + 1);
                other.tdbg(n + 1);
            }
        }
    }
}
impl TDbg for Body {
    fn tdbg(&self, n: usize) {
        match &self {
            Body { defs, exps, ret } => {
                Self::pln(n, "Body");
                defs.tdbg(n + 1);
                exps.tdbg(n + 1);
                ret.tdbg(n + 1);
            }
        }
    }
}
impl TDbg for Id {
    fn tdbg(&self, n: usize) {
        Self::indent(n);
        println!("{:?}", self);
    }
}
impl TDbg for Const {
    fn tdbg(&self, n: usize) {
        Self::indent(n);
        println!("{:?}", self);
    }
}
impl TDbg for SExp {
    fn tdbg(&self, n: usize) {
        Self::pln(n, "SExp");
        match self {
            SExp::Const(c) => {
                c.tdbg(n + 1);
            }
            SExp::Id(id) => {
                id.tdbg(n + 1);
            }
            SExp::List { elems, tail } => {
                elems.tdbg(n + 1);
                match tail {
                    None => Self::pln(n + 1, "None"),
                    Some(v) => {
                        Self::p(n + 1, "Some");
                        v.tdbg(0)
                    }
                }
            }
        }
    }
}
