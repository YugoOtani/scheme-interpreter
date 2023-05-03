use crate::{env::*, token::*};
use std::cell::*;
use std::rc::Rc;
type EResult = Result<Rc<SchemeVal>, String>;

impl Toplevel {
    pub fn eval(&self, env: &mut Env) -> EResult {
        match self {
            Toplevel::Define(def) => def.eval(env),
            Toplevel::Exp(exp) => exp.eval(env),
            Toplevel::Load(_) => unreachable!(),
        }
    }
}
impl Define {
    fn eval(&self, env: &mut Env) -> EResult {
        match self {
            Define::Var(id, exp) => {
                let v = exp.eval(env)?;
                env.get_frame().borrow_mut().insert(id, &v);
                Ok(Rc::new(SchemeVal::None))
            }
            Define::Func(id, param, body) => {
                let closure = SchemeVal::Closure(env.get_frame(), param.clone(), body.clone());
                env.get_frame().borrow_mut().insert(id, &Rc::new(closure));
                Ok(Rc::new(SchemeVal::None))
            }
        }
    }
}
impl Exp {
    fn eval(&self, env: &mut Env) -> EResult {
        match self {
            Exp::Const(c) => c.eval(),
            Exp::Id(id) => id.eval(env),
            Exp::FunCall { fname, args } => {
                let func = fname.eval(env)?;
                let mut args_v = vec![];
                for e in args {
                    let v = e.eval(env)?;
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
                        let old_frame = env.get_frame();
                        env.set_frame(eval_frame);
                        let Body { defs, exps, ret } = body.clone();
                        for def in defs {
                            def.eval(env)?;
                        }
                        for exp in &exps {
                            exp.eval(env)?;
                        }
                        let ret = ret.eval(env);
                        env.set_frame(old_frame);
                        ret
                    }
                    _ => panic!(),
                }
            }
            Exp::Lambda(prms, body) => {
                let closure = SchemeVal::Closure(env.get_frame(), prms.clone(), body.clone());
                Ok(Rc::new(closure))
            }
            Exp::Set(id, exp) => {
                let v = exp.eval(env)?;
                env.get_frame()
                    .borrow_mut()
                    .replace(id, &v)
                    .ok_or(format!("could not find value {}", id.get()))?;
                Ok(Rc::new(SchemeVal::None))
            }
            Exp::Quote(sexp) => sexp.eval(env),
            Exp::Let {
                name: None,
                bind,
                body: Body { defs, exps, ret },
            } => {
                let current_frame = env.get_frame();
                let new_frame = Frame::empty(&current_frame);
                for Bind { name, val } in bind {
                    let v = val.eval(env)?; // in current_frame
                    new_frame
                        .borrow_mut()
                        .insert_new(&name, &v)
                        .ok_or("[let] cannot use the same parameter in let bindings")?;
                }
                env.set_frame(new_frame);
                for def in defs {
                    def.eval(env)?;
                }
                for exp in exps {
                    exp.eval(env)?;
                }
                let r = ret.eval(env)?;
                env.set_frame(current_frame);
                Ok(r)
            }
            // TODO: defnew  <- not to appear the same id in the left hand
            /*Exp::LetRec(bind, Body { defs, exps, ret }) => {
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
            }*/
            Exp::If {
                cond,
                then_exp,
                else_exp,
            } => {
                let cond = cond.eval(env)?;
                let b = match cond.as_ref() {
                    SchemeVal::Bool(b) => b,
                    _ => return Err(format!("condition of if statement is not bool")),
                };
                if *b {
                    then_exp.eval(env)
                } else {
                    match else_exp {
                        None => Ok(Rc::new(SchemeVal::None)),
                        Some(v) => v.eval(env),
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
                                    exp.eval(env)?;
                                }
                                break ret.eval(env);
                            }
                            None => break Ok(Rc::new(SchemeVal::None)),
                        },
                        Some(Branch { cond, then, ret }) => {
                            let cond = cond.eval(env)?;
                            match cond.as_ref() {
                                SchemeVal::Bool(b) => {
                                    if *b {
                                        for exp in then {
                                            exp.eval(env)?;
                                        }
                                        break ret.eval(env);
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
                        match exp.eval(env)?.as_ref() {
                            SchemeVal::Bool(false) => return Ok(Rc::new(SchemeVal::Bool(false))),
                            _ => continue,
                        }
                    }
                    last.eval(env)
                }
            },
            Exp::Or(exps) => match &exps[..] {
                [] => Ok(Rc::new(SchemeVal::Bool(false))),
                v => {
                    let (last, elems) = v.split_last().unwrap();
                    for exp in elems {
                        let v = exp.eval(env)?;
                        match v.as_ref() {
                            SchemeVal::Bool(false) => continue,
                            _ => return Ok(v),
                        }
                    }
                    last.eval(env)
                }
            },
            _ => todo!(),
        }
    }
}
fn params_args(p: Params, args: Vec<Rc<SchemeVal>>) -> Result<Vec<(Id, Rc<SchemeVal>)>, String> {
    let Params { prms, other } = p;
    if prms.len() > args.len() {
        return Err("number of argument is incorrect".to_string());
    }
    match other {
        Some(id) => {
            let mut ret = vec![];
            let n = args.len();
            for (prm, arg) in prms.iter().zip(args.iter()) {
                ret.push((prm.clone(), arg.clone()));
            }
            ret.push((id, SchemeVal::from_list(&args[n..], None)));
            return Ok(ret);
        }
        None => {
            if prms.len() != args.len() {
                return Err("number of argument is incorrect".to_string());
            }
            let mut ret = vec![];
            for (prm, arg) in prms.iter().zip(args.iter()) {
                ret.push((prm.clone(), arg.clone()));
            }
            return Ok(ret);
        }
    }
}

impl Const {
    fn eval(&self) -> EResult {
        Ok(match self {
            Const::Bool(b) => Rc::new(SchemeVal::Bool(*b)),
            Const::String(ref s) => Rc::new(SchemeVal::String(s.to_string())),
            Const::Num(n) => Rc::new(SchemeVal::Num(*n)),
            Const::Nil => Rc::new(SchemeVal::Nil),
        })
    }
}
impl Id {
    fn eval(&self, env: &mut Env) -> EResult {
        let v = env.get_frame().borrow().find(self).ok_or(format!(
            "could not find value {} \n note: frame is\n {}",
            self.get(),
            env.get_frame().as_ref().borrow().to_string()
        ))?;
        Ok(Rc::clone(&v))
    }
}
impl SExp {
    fn eval(&self, env: &mut Env) -> EResult {
        match self {
            SExp::Const(c) => c.eval(),
            SExp::Id(id) => {
                env.add_sym(id);
                env.get_sym(id)
                    .ok_or(format!("could not find Symbol {}", id.get()))
            }
            SExp::List { elems, tail } => {
                let tailv = match *tail {
                    None => None,
                    Some(ref v) => Some(v.eval(env)?),
                };
                let mut lst = vec![];
                for e in elems {
                    lst.push(e.eval(env)?);
                }
                Ok(SchemeVal::from_list(&lst[..], tailv))
            }
        }
    }
}
