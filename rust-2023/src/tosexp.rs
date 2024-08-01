use crate::token::*;
pub trait ToSExp {
    fn to_sexp(&self) -> SExp;
}
impl<T> ToSExp for Vec<T>
where
    T: ToSExp,
{
    fn to_sexp(&self) -> SExp {
        let elems: Vec<SExp> = self.iter().map(|exp| exp.to_sexp()).collect();
        SExp::list(elems)
    }
}

impl ToSExp for Branch {
    fn to_sexp(&self) -> SExp {
        let mut v = vec![];
        let Branch { cond, then, ret } = self;
        v.push(cond.to_sexp());
        for exp in then {
            v.push(exp.to_sexp());
        }
        v.push(ret.to_sexp());
        SExp::list(v)
    }
}
impl ToSExp for Define {
    fn to_sexp(&self) -> SExp {
        match self {
            Define::Func(id, params, body) => {
                let mut v = vec![];

                v.push(id.to_sexp());
                v.push(params.to_sexp());
                for exps in body.to_sexp() {
                    v.push(exps)
                }
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }

            Define::Var(id, exp) => {
                SExp::list(vec!["define".to_sexp(), id.to_sexp(), exp.to_sexp()])
            }
        }
    }
}

impl ToSExp for Id {
    fn to_sexp(&self) -> SExp {
        SExp::Id(self.clone())
    }
}
impl ToSExp for SExp {
    fn to_sexp(&self) -> SExp {
        self.clone()
    }
}
impl ToSExp for &str {
    fn to_sexp(&self) -> SExp {
        SExp::Id(Id::new(self).unwrap())
    }
}
impl ToSExp for Params {
    fn to_sexp(&self) -> SExp {
        let Params { prms, other } = self;
        let mut v2 = vec![];
        for prm in prms {
            v2.push(prm.to_sexp());
        }
        SExp::List {
            elems: v2,
            tail: other.clone().map(|id| Box::new(SExp::Id(id))),
        }
    }
}
impl Body {
    fn to_sexp(&self) -> Vec<SExp> {
        let mut v = vec![];
        let Body { defs, exps, ret } = self;
        for def in defs {
            v.push(def.to_sexp())
        }
        for exp in exps {
            v.push(exp.to_sexp())
        }
        v.push(ret.to_sexp());
        v
    }
}
impl SExp {
    pub fn list(v: Vec<SExp>) -> SExp {
        SExp::List {
            elems: v,
            tail: None,
        }
    }
}
impl ToSExp for Exp {
    fn to_sexp(&self) -> SExp {
        match self {
            Exp::Const(c) => c.to_sexp(),
            Exp::Id(id) => id.to_sexp(),
            Exp::Lambda(prms, body) => {
                let mut v = vec!["lambda".to_sexp()];
                v.push(prms.to_sexp());
                for exp in body.to_sexp() {
                    v.push(exp)
                }
                SExp::list(v)
            }
            Exp::FunCall { fname, args } => {
                let mut v = vec![];
                v.push(fname.to_sexp());
                for arg in args {
                    v.push(arg.to_sexp())
                }
                SExp::list(v)
            }
            Exp::Quote(sexp) => SExp::list(vec!["quote".to_sexp(), sexp.to_sexp()]),
            Exp::Set(id, exp) => SExp::list(vec!["set!".to_sexp(), id.to_sexp(), exp.to_sexp()]),
            Exp::Let { name, bind, body } => match name {
                Some(name) => {
                    let mut v = vec!["let".to_sexp(), name.to_sexp(), bind.to_sexp()];
                    for exp in body.to_sexp() {
                        v.push(exp)
                    }
                    SExp::list(v)
                }

                None => {
                    let mut v = vec!["let".to_sexp(), bind.to_sexp()];
                    for exp in body.to_sexp() {
                        v.push(exp)
                    }
                    SExp::list(v)
                }
            },
            Exp::Let2(bind, body) => {
                let mut v = vec!["let*".to_sexp(), bind.to_sexp()];
                for exp in body.to_sexp() {
                    v.push(exp)
                }
                SExp::list(v)
            }
            Exp::LetRec(bind, body) => {
                let mut v = vec!["letrec".to_sexp(), bind.to_sexp()];
                for exp in body.to_sexp() {
                    v.push(exp)
                }
                SExp::list(v)
            }
            Exp::If {
                cond,
                then_exp,
                else_exp,
            } => match else_exp {
                Some(exp) => SExp::List {
                    elems: vec![
                        "if".to_sexp(),
                        cond.to_sexp(),
                        then_exp.to_sexp(),
                        "else".to_sexp(),
                        exp.to_sexp(),
                    ],
                    tail: None,
                },
                None => SExp::List {
                    elems: vec!["if".to_sexp(), cond.to_sexp(), then_exp.to_sexp()],
                    tail: None,
                },
            },

            Exp::Cond {
                branches,
                else_branch,
            } => {
                let mut v = vec![];
                v.push("cond".to_sexp());
                for b in branches {
                    v.push(b.to_sexp());
                }
                if let Some((mut exps, ret)) = else_branch.clone() {
                    exps.push(*ret);
                    v.push(exps.to_sexp());
                }
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }
            Exp::And(exps) => {
                let mut v = vec![];
                v.push("and".to_sexp());
                for b in exps {
                    v.push(b.to_sexp());
                }
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }
            Exp::Or(exps) => {
                let mut v = vec![];
                v.push("or".to_sexp());
                for b in exps {
                    v.push(b.to_sexp());
                }
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }
            Exp::Begin(exps) => {
                let mut v = vec![];
                v.push("begin".to_sexp());
                for b in exps {
                    v.push(b.to_sexp());
                }
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }
            Exp::DefMacro(id, prms, body) => {
                let mut v = vec![];

                v.push("macro".to_sexp());
                v.push(id.to_sexp());
                v.push(prms.to_sexp());
                v.push(body.to_sexp());
                SExp::List {
                    elems: v,
                    tail: None,
                }
            }
            Exp::ExpandMacro(id, sexp) => {
                let mut v = vec![id.to_sexp()];
                for e in sexp {
                    v.push(e.to_sexp())
                }
                SExp::list(v)
            }
            Exp::Do {
                binds,
                pred,
                ret,
                body,
            } => {
                let mut v = vec![];
                v.push("do".to_sexp());
                v.push(binds.to_sexp());
                v.push(SExp::list(vec![pred.to_sexp(), ret.to_sexp()]));
                for e in body.to_sexp() {
                    v.push(e)
                }
                SExp::list(v)
            }
        }
    }
}
impl ToSExp for DoBind {
    fn to_sexp(&self) -> SExp {
        let DoBind { name, init, update } = self;
        SExp::list(vec![name.to_sexp(), init.to_sexp(), update.to_sexp()])
    }
}
impl ToSExp for Const {
    fn to_sexp(&self) -> SExp {
        SExp::Const(self.clone())
    }
}
impl ToSExp for Bind {
    fn to_sexp(&self) -> SExp {
        let Bind { name, val } = self;
        SExp::List {
            elems: vec![name.to_sexp(), val.to_sexp()],
            tail: None,
        }
    }
}
