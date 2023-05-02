use crate::{frame::*, token::*};
use std::cell::*;
use std::collections::HashMap;
use std::rc::Rc;

type EResult = Result<Rc<SchemeVal>, String>;

impl Toplevel {
    pub fn eval(&self, frame: Rc<RefCell<Frame>>) -> EResult {
        match self {
            Toplevel::Define(def) => def.eval(frame),
            Toplevel::Exp(exp) => exp.eval(frame),
            Toplevel::Load(_) => unreachable!(),
        }
    }
}
impl Define {
    fn eval(&self, frame: Rc<RefCell<Frame>>) -> EResult {
        match self {
            Define::Var(id, exp) => {
                let v = exp.eval(frame.clone())?;
                let id = id.get();
                frame.clone().borrow_mut().insert(id, &v);
                Ok(Rc::new(SchemeVal::None))
            }
            Define::Func(id, param, body) => {
                let id = id.get();
                let closure = SchemeVal::Closure(frame.clone(), param.clone(), body.clone());
                frame.borrow_mut().insert(id, &Rc::new(closure));
                Ok(Rc::new(SchemeVal::None))
            }
        }
    }
}
impl Exp {
    fn eval(&self, frame: Rc<RefCell<Frame>>) -> EResult {
        match self {
            Exp::Const(c) => c.eval(frame),
            Exp::Id(id) => id.eval(frame),
            Exp::FunCall { fname, args } => {
                let func = fname.eval(frame.clone())?;
                let mut args_v = vec![];
                for e in args {
                    let v = e.eval(frame.clone())?;
                    args_v.push(v);
                }

                match *func.clone() {
                    SchemeVal::RootFn(ref f) => Ok(f(args_v)?),
                    SchemeVal::Closure(ref parframe, ref params, ref body) => {
                        let p_a = params_args(params.clone(), args_v)?;
                        let eval_frame = Frame {
                            vars: p_a.into_iter().collect(),
                            par: Some(parframe.clone()),
                        };
                        let eval_frame = Rc::new(RefCell::new(eval_frame));
                        let Body { defs, exps, ret } = body.clone();
                        for def in defs {
                            def.eval(eval_frame.clone())?;
                        }
                        for exp in &exps {
                            exp.eval(eval_frame.clone())?;
                        }
                        ret.eval(eval_frame)
                    }
                    _ => panic!(),
                }
            }
            Exp::Lambda(prms, body) => {
                let closure = SchemeVal::Closure(frame.clone(), prms.clone(), body.clone());

                Ok(Rc::new(closure))
            }
            Exp::Set(id, exp) => {
                let id = id.get();
                let v = exp.eval(frame.clone())?;
                frame
                    .clone()
                    .borrow_mut()
                    .replace(id, &v)
                    .ok_or(format!("could not find value {id}"))?;
                Ok(Rc::new(SchemeVal::None))
            }
            Exp::Quote(sexp) => sexp.eval(frame.clone()),

            // TODO: defnew
            Exp::LetRec(bind, Body { defs, exps, ret }) => {
                let frame = Frame {
                    vars: HashMap::new(),
                    par: Some(frame.clone()),
                };

                let frame = Rc::new(RefCell::new(frame));
                for Bind { name, val } in bind {
                    let val = val.eval(frame.clone())?;
                    let name = name.get().to_string();
                    frame.clone().as_ref().borrow_mut().insert(&name, &val);
                }

                for def in defs {
                    def.eval(frame.clone())?;
                }
                for exp in exps {
                    exp.eval(frame.clone())?;
                }
                ret.eval(frame.clone())
            }
            Exp::If {
                cond,
                then_exp,
                else_exp,
            } => {
                let cond = cond.eval(frame.clone())?;
                let b = match cond.as_ref() {
                    SchemeVal::Bool(b) => b,
                    _ => return Err(format!("condition of if statement is not bool")),
                };
                if *b {
                    then_exp.eval(frame.clone())
                } else {
                    match else_exp {
                        None => Ok(Rc::new(SchemeVal::None)),
                        Some(v) => v.eval(frame.clone()),
                    }
                }
            }
            Exp::Cond {
                branches,
                else_branch,
            } => {
                let mut ite = branches.iter();
                loop {
                    match ite.next() {
                        None => match else_branch {
                            Some((exps, ret)) => {
                                for exp in exps {
                                    exp.eval(frame.clone())?;
                                }
                                break ret.eval(frame.clone());
                            }
                            None => break Ok(Rc::new(SchemeVal::None)),
                        },
                        Some(Branch { cond, then, ret }) => {
                            let cond = cond.eval(frame.clone())?;
                            match cond.as_ref() {
                                SchemeVal::Bool(b) => {
                                    if *b {
                                        for exp in then {
                                            exp.eval(frame.clone())?;
                                        }
                                        break ret.eval(frame.clone());
                                    }
                                }
                                _ => break Err(format!("[cond] condition must be boolean")),
                            }
                        }
                    }
                }
            }
            Exp::And(exps) => match &exps[..] {
                [] => Ok(Rc::new(SchemeVal::Bool(true))),
                v => {
                    let (last, elems) = v.split_last().unwrap();
                    for exp in elems {
                        match exp.eval(frame.clone())?.as_ref() {
                            SchemeVal::Bool(false) => return Ok(Rc::new(SchemeVal::Bool(false))),
                            _ => continue,
                        }
                    }
                    last.eval(frame.clone())
                }
            },
            Exp::Or(exps) => match &exps[..] {
                [] => Ok(Rc::new(SchemeVal::Bool(false))),
                v => {
                    let (last, elems) = v.split_last().unwrap();
                    for exp in elems {
                        let v = exp.eval(frame.clone())?;
                        match v.as_ref() {
                            SchemeVal::Bool(false) => continue,
                            _ => return Ok(v),
                        }
                    }
                    last.eval(frame.clone())
                }
            },
            _ => todo!(),
        }
    }
}
fn params_args(p: Params, arg: Vec<Rc<SchemeVal>>) -> Result<Vec<(String, Rc<SchemeVal>)>, String> {
    let Params { prms, other } = p;
    if prms.len() > arg.len() {
        return Err("number of argument is incorrect".to_string());
    }
    match other {
        Some(id) => {
            let mut ret = vec![];
            let n = arg.len();
            for i in 0..n {
                ret.push((prms[i].get().to_string(), arg[i].clone()));
            }
            ret.push((id.get().to_string(), SchemeVal::from_list(&arg[n..], None)));
            return Ok(ret);
        }
        None => {
            if prms.len() != arg.len() {
                return Err("number of argument is incorrect".to_string());
            }
            let mut ret = vec![];
            for i in 0..prms.len() {
                ret.push((prms[i].get().to_string(), arg[i].clone()));
            }
            return Ok(ret);
        }
    }
}

impl Const {
    fn eval(&self, _: Rc<RefCell<Frame>>) -> EResult {
        Ok(match self {
            Const::Bool(b) => Rc::new(SchemeVal::Bool(*b)),
            Const::String(ref s) => Rc::new(SchemeVal::String(s.to_string())),
            Const::Num(n) => Rc::new(SchemeVal::Num(*n)),
            Const::Nil => Rc::new(SchemeVal::Nil),
        })
    }
}
impl Id {
    fn eval(&self, frame: Rc<RefCell<Frame>>) -> EResult {
        let k = self.get();
        let e = frame.clone();
        let r = e.as_ref();
        let v = r.borrow().find(k).ok_or(format!(
            "could not find value {k} \n note: frame is\n {}",
            frame.clone().as_ref().borrow().to_string()
        ))?;
        Ok(Rc::clone(&v))
    }
}
impl SExp {
    fn eval(&self, frame: Rc<RefCell<Frame>>) -> EResult {
        match self {
            SExp::Const(c) => c.eval(frame),
            SExp::Id(id) => Ok(Rc::new(SchemeVal::Sym(id.clone()))),
            SExp::List { elems, tail } => {
                let tailv = match *tail {
                    None => None,
                    Some(ref v) => Some(v.eval(frame.clone())?),
                };
                let mut lst = vec![];
                for e in elems {
                    lst.push(e.eval(frame.clone())?);
                }
                Ok(SchemeVal::from_list(&lst[..], tailv))
            }
        }
    }
}
