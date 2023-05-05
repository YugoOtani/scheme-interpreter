use crate::{env::*, token::*};
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
        let mut exp = self.clone();
        let initial_frame = env.get_frame();
        loop {
            match exp {
                Exp::Const(c) => {
                    let c = c.eval();
                    env.set_frame(initial_frame);
                    break c;
                }
                Exp::Id(id) => {
                    let v = id.eval(env);
                    env.set_frame(initial_frame);
                    break v;
                }
                Exp::FunCall { fname, args } => {
                    let func = fname.eval(env)?;
                    let mut args_v = vec![];
                    for e in args {
                        let v = e.eval(env)?;
                        args_v.push(v);
                    }
                    let f = func.clone();
                    match f.as_ref() {
                        SchemeVal::RootFn(f) => break Ok(f(args_v)?),
                        SchemeVal::Closure(parframe, params, Body { defs, exps, ret }) => {
                            let p_a = params_args(params.clone(), args_v)?;
                            let eval_frame = Frame::empty(parframe);
                            for (id, val) in &p_a {
                                eval_frame
                                    .borrow_mut()
                                    .insert_new(id, val)
                                    .ok_or("cannot use same parameter in function")?;
                            }

                            env.set_frame(eval_frame);
                            for def in defs {
                                def.eval(env)?;
                            }
                            for exp in exps {
                                exp.eval(env)?;
                            }
                            exp = *ret.clone();
                            continue;
                        }
                        _ => break Err(format!("result of {:?} is not a function", fname)),
                    }
                }
                Exp::Lambda(prms, body) => {
                    let closure = SchemeVal::Closure(env.get_frame(), prms.clone(), body.clone());
                    env.set_frame(initial_frame);
                    break Ok(Rc::new(closure));
                }
                Exp::Set(id, exp) => {
                    let v = exp.eval(env)?;
                    env.get_frame()
                        .borrow_mut()
                        .replace(&id, &v)
                        .ok_or(format!("could not find value {}", id.get()))?;
                    env.set_frame(initial_frame);
                    break Ok(Rc::new(SchemeVal::None));
                }
                Exp::Quote(sexp) => {
                    let r = sexp.eval(env);
                    env.set_frame(initial_frame);
                    break r;
                }

                Exp::Let { name, bind, body } => {
                    let new_frame = Frame::empty(&env.get_frame());
                    for Bind { name, val } in &bind {
                        let v = val.eval(env)?; // in current_frame
                        new_frame
                            .borrow_mut()
                            .insert_new(&name, &v)
                            .ok_or("[let] cannot use the same parameter in let bindings")?;
                    }
                    env.set_frame(new_frame);
                    if let Some(name) = name {
                        let prms = bind
                            .iter()
                            .map(|Bind { name, val: _ }| name.clone())
                            .collect();
                        let closure = SchemeVal::Closure(
                            env.get_frame(),
                            Params { prms, other: None },
                            body.clone(),
                        );
                        env.get_frame()
                            .borrow_mut()
                            .insert_new(&name, &Rc::new(closure))
                            .ok_or("[named-let] name {} is already used")?;
                    }
                    let Body { defs, exps, ret } = body;
                    for def in defs {
                        def.eval(env)?;
                    }
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = *ret;
                    continue;
                }
                //(let ((x 1)) (let* ((y (lambda() x)) (x 2)) (y))) ;1
                Exp::Let2(binds, Body { defs, exps, ret }) => {
                    let2_env(&binds, env)?;
                    for def in defs {
                        def.eval(env)?;
                    }
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = *ret.clone();
                    continue;
                }

                Exp::LetRec(bind, Body { defs, exps, ret }) => {
                    let frame = env.get_frame();
                    let new_frame = Frame::empty(&frame);
                    for Bind { name, val: _ } in &bind {
                        new_frame
                            .borrow_mut()
                            .alloc_new(&name)
                            .ok_or("[letrec] cannot use the same variable in bindings")?;
                    }
                    env.set_frame(new_frame);
                    let mut vals = vec![];
                    for Bind { name: _, val } in &bind {
                        vals.push(val.eval(env)?);
                    }
                    for (name, val) in bind
                        .iter()
                        .zip(vals.iter())
                        .map(|(Bind { name, val: _ }, v)| (name, v))
                    {
                        env.get_frame().borrow_mut().replace(name, val).unwrap()
                    }
                    for def in defs {
                        def.eval(env)?;
                    }
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = *ret;
                    continue;
                }
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
                        exp = *then_exp;
                        continue;
                    } else {
                        match else_exp {
                            None => {
                                env.set_frame(initial_frame);
                                break Ok(Rc::new(SchemeVal::None));
                            }
                            Some(v) => {
                                exp = *v;
                                continue;
                            }
                        }
                    }
                }
                Exp::Cond {
                    branches,
                    else_branch,
                } => {
                    let mut ite = branches.iter();
                    exp = loop {
                        match ite.next() {
                            None => match else_branch {
                                Some((exps, ret)) => {
                                    for exp in exps {
                                        exp.eval(env)?;
                                    }
                                    break *ret;
                                }
                                None => {
                                    env.set_frame(initial_frame);
                                    return Ok(Rc::new(SchemeVal::None));
                                }
                            },
                            Some(Branch { cond, then, ret }) => {
                                let cond = cond.eval(env)?;
                                match cond.as_ref() {
                                    SchemeVal::Bool(b) => {
                                        if *b {
                                            for exp in then {
                                                exp.eval(env)?;
                                            }
                                            break ret.clone();
                                        }
                                    }
                                    _ => return Err(format!("[cond] condition must be boolean")),
                                }
                            }
                        }
                    };
                    continue;
                }
                Exp::And(exps) => match &exps[..] {
                    [] => {
                        env.set_frame(initial_frame);
                        break Ok(Rc::new(SchemeVal::Bool(true)));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            match exp.eval(env)?.as_ref() {
                                SchemeVal::Bool(false) => {
                                    env.set_frame(initial_frame);
                                    return Ok(Rc::new(SchemeVal::Bool(false)));
                                }
                                _ => continue,
                            }
                        }
                        exp = last.clone();
                        continue;
                    }
                },
                Exp::Or(exps) => match &exps[..] {
                    [] => {
                        env.set_frame(initial_frame);
                        break Ok(Rc::new(SchemeVal::Bool(false)));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            let v = exp.eval(env)?;
                            match v.as_ref() {
                                SchemeVal::Bool(false) => continue,
                                _ => {
                                    env.set_frame(initial_frame);
                                    return Ok(v);
                                }
                            }
                        }
                        exp = last.clone();
                        continue;
                    }
                },
                Exp::Begin(exps) => {
                    let (last, exps) = exps
                        .split_last()
                        .ok_or("begin must at least have one argument")?;
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = last.clone();
                    continue;
                }
            }
        }
    }
}

fn let2_env(binds: &[Bind], env: &mut Env) -> Result<(), String> {
    match binds {
        [] => Ok(()),
        [Bind { name, val }, tail @ ..] => {
            let val = val.eval(env)?;
            let par = env.get_frame();
            env.set_frame(Frame::empty(&par));
            env.get_frame().borrow_mut().insert_new(name, &val).unwrap();
            let2_env(tail, env)
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
        let v = env
            .get_frame()
            .as_ref()
            .borrow()
            .find(self)
            .ok_or(format!("could not find value {} \n", self.get()))?;
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
