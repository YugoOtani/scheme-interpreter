use crate::{env::*, token::*};
use std::cell::*;
use std::collections::HashMap;
use std::rc::Rc;

type EResult = Result<Rc<SchemeVal>, String>;

impl Toplevel {
    pub fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        match self {
            Toplevel::Define(def) => def.eval(env),
            Toplevel::Exp(exp) => exp.eval(env),
            Toplevel::Load(_) => panic!("load must be defined in main.rs"),
        }
    }
}
impl Define {
    fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        match self {
            Define::Var(id, exp) => {
                let v = exp.eval(env.clone())?;
                let id = id.get();
                env.clone().borrow_mut().insert(id, &v);
                Ok(Rc::new(SchemeVal::None))
            }
            Define::Func(id, param, body) => {
                let id = id.get();
                let closure = SchemeVal::Closure(env.clone(), param.clone(), body.clone());
                env.borrow_mut().insert(id, &Rc::new(closure));
                Ok(Rc::new(SchemeVal::None))
            }
        }
    }
}
impl Exp {
    fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        match self {
            Exp::Const(c) => c.eval(env),
            Exp::Id(id) => id.eval(env),
            Exp::FunCall { fname, args } => {
                let func = fname.eval(env.clone())?;
                let mut args_v = vec![];
                for e in args {
                    let v = e.eval(env.clone())?;
                    args_v.push(v);
                }
                match *func.clone() {
                    SchemeVal::RootFn(ref f) => Ok(f(args_v)?),
                    SchemeVal::Closure(ref parenv, ref params, ref body) => {
                        let p_a = params_args(params.clone(), args_v)?;
                        let eval_frame = Env {
                            vars: p_a.into_iter().collect(),
                            par: Some(parenv.clone()),
                        };
                        let eval_env = Rc::new(RefCell::new(eval_frame));
                        let Body { defs, exps, ret } = body.clone();
                        for def in defs {
                            def.eval(eval_env.clone())?;
                        }
                        for exp in &exps {
                            exp.eval(eval_env.clone())?;
                        }
                        ret.eval(eval_env)
                    }
                    _ => panic!(),
                }
            }
            Exp::Lambda(prms, body) => {
                let closure = SchemeVal::Closure(env.clone(), prms.clone(), body.clone());
                Ok(Rc::new(closure))
            }
            Exp::Set(id, exp) => {
                let id = id.get();
                let v = exp.eval(env.clone())?;
                env.clone()
                    .borrow_mut()
                    .replace(id, &v)
                    .ok_or(format!("could not find value {id}"))?;
                Ok(Rc::new(SchemeVal::None))
            }
            Exp::Quote(sexp) => sexp.eval(env.clone()),
            Exp::LetRec(bind, Body { defs, exps, ret }) => {
                let env = Env {
                    vars: HashMap::new(),
                    par: Some(env.clone()),
                };
                let env = Rc::new(RefCell::new(env));
                for Bind { name, val } in bind {
                    let val = val.eval(env.clone())?;
                    let name = name.get().to_string();
                    env.clone().as_ref().borrow_mut().insert(&name, &val);
                }

                for def in defs {
                    def.eval(env.clone())?;
                }
                for exp in exps {
                    exp.eval(env.clone())?;
                }
                ret.eval(env.clone())
            }
            Exp::If {
                cond,
                then_exp,
                else_exp,
            } => {
                let cond = cond.eval(env.clone())?;
                let b = match cond.as_ref() {
                    SchemeVal::Bool(b) => b,
                    _ => return Err(format!("condition of if statement is not bool")),
                };
                if *b {
                    then_exp.eval(env.clone())
                } else {
                    match else_exp {
                        None => Ok(Rc::new(SchemeVal::None)),
                        Some(v) => v.eval(env.clone()),
                    }
                }
            }
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
    fn eval(&self, _: Rc<RefCell<Env>>) -> EResult {
        Ok(match self {
            Const::Bool(b) => Rc::new(SchemeVal::Bool(*b)),
            Const::String(ref s) => Rc::new(SchemeVal::String(s.to_string())),
            Const::Num(n) => Rc::new(SchemeVal::Num(*n)),
            Const::Nil => Rc::new(SchemeVal::Nil),
        })
    }
}
impl Id {
    fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        let k = self.get();
        let e = env.clone();
        let r = e.as_ref();
        let v = r.borrow().find(k).ok_or(format!(
            "could not find value {k} \n note: env is\n {}",
            env.clone().as_ref().borrow().to_string()
        ))?;
        Ok(Rc::clone(&v))
    }
}
impl SExp {
    fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        match self {
            SExp::Const(c) => c.eval(env),
            SExp::Id(id) => Ok(Rc::new(SchemeVal::Sym(id.clone()))),
            SExp::List { elems, tail } => {
                let tailv = match *tail {
                    None => None,
                    Some(ref v) => Some(v.eval(env.clone())?),
                };
                let mut lst = vec![];
                for e in elems {
                    lst.push(e.eval(env.clone())?);
                }
                Ok(SchemeVal::from_list(&lst[..], tailv))
            }
        }
    }
}
