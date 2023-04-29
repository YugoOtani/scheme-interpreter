use crate::{env::*, token::*};
use std::cell::*;
use std::rc::Rc;

type EResult = Result<Rc<SchemeVal>, String>;

impl Toplevel {
    pub fn eval(self, env: Rc<RefCell<Env>>) -> EResult {
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
                let v = exp.clone().eval(env.clone())?;
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
    fn eval(self, env: Rc<RefCell<Env>>) -> EResult {
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
                    SchemeVal::RootFn(ref f) => Ok(Rc::new(f(args_v)?)),
                    SchemeVal::Closure(ref parenv, ref params, ref body) => {
                        let p_a = params_args(params.clone(), args_v)?;
                        let eval_frame = Env {
                            vars: p_a.into_iter().collect(),
                            par: Some(parenv.clone()),
                        };
                        let eval_env = Rc::new(RefCell::new(eval_frame));
                        let Body { defs, exps } = body.clone();
                        for def in defs {
                            def.eval(eval_env.clone())?;
                        }
                        let n = exps.len();
                        for exp in &exps {
                            exp.clone().eval(eval_env.clone())?;
                        }
                        exps[n - 1].clone().eval(eval_env)
                    }
                    _ => panic!(),
                }
            }
            Exp::Lambda(prms, body) => {
                let closure = SchemeVal::Closure(env.clone(), prms, body);
                Ok(Rc::new(closure))
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
            ret.push((
                id.get().to_string(),
                Rc::new(SchemeVal::from_list(&arg[n..])),
            ));
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
    fn eval(self, _: Rc<RefCell<Env>>) -> EResult {
        Ok(match self {
            Const::Bool(b) => Rc::new(SchemeVal::Bool(b)),
            Const::String(ref s) => Rc::new(SchemeVal::String(s.to_string())),
            Const::Num(n) => Rc::new(SchemeVal::Num(n)),
            Const::Nil => Rc::new(SchemeVal::Nil),
        })
    }
}
impl Id {
    fn eval(&self, env: Rc<RefCell<Env>>) -> EResult {
        let k = self.get();
        let e = env.clone();
        let eb = e.borrow();
        let v = eb.find(k).ok_or(format!("could not find value {k}"))?;
        Ok(Rc::clone(&v))
    }
}
impl SExp {
    fn eval(self, env: Rc<RefCell<Env>>) -> EResult {
        todo!()
    }
}
