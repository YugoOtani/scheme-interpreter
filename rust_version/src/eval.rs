use crate::{env::*, token::*};
use anyhow::{bail, ensure, Context, Result};
type EResult = Result<V>;

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
                Ok(V::new(SchemeVal::None))
            }
            Define::Func(id, param, body) => {
                let closure = SchemeVal::Closure(env.get_frame(), param.clone(), body.clone());
                env.get_frame().borrow_mut().insert(id, &V::new(closure));
                Ok(V::new(SchemeVal::None))
            }
        }
    }
}
impl Exp {
    fn eval(&self, env: &mut Env) -> EResult {
        let mut exp = self.clone();
        let initial_frame = env.get_frame();
        loop {
            // loop for tail recursion
            match exp {
                Exp::Const(c) => {
                    env.set_frame(initial_frame);
                    break c.eval();
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
                    match &*f.get().borrow() {
                        SchemeVal::RootFn(f) => {
                            env.set_frame(initial_frame);
                            break f(args_v, env);
                        }
                        SchemeVal::Closure(parframe, params, Body { defs, exps, ret }) => {
                            let p_a = params_args(params.clone(), args_v)?;
                            let eval_frame = Frame::empty(&parframe);
                            for (id, val) in &p_a {
                                eval_frame
                                    .borrow_mut()
                                    .insert_new(id, val)
                                    .context("cannot use the same parameter in function")?;
                            }

                            env.set_frame(eval_frame);
                            for def in defs {
                                def.eval(env)?;
                            }
                            for exp in exps {
                                exp.eval(env)?;
                            }
                            exp = *ret.clone();
                        }
                        _ => bail!("result of {:?} is not a function", fname),
                    }
                }
                Exp::Lambda(prms, body) => {
                    let closure = SchemeVal::Closure(env.get_frame(), prms.clone(), body.clone());
                    env.set_frame(initial_frame);
                    break Ok(V::new(closure));
                }
                Exp::Set(id, exp) => {
                    let v = exp.eval(env)?;
                    env.get_frame()
                        .borrow_mut()
                        .replace(&id, &v)
                        .with_context(|| format!("could not find value {}", id.get()))?;
                    env.set_frame(initial_frame);
                    break Ok(V::new(SchemeVal::None));
                }
                Exp::Quote(sexp) => {
                    let r = sexp.eval(env);
                    env.set_frame(initial_frame);
                    break r;
                }

                Exp::Let { name, bind, body } => {
                    let new_frame = Frame::empty(&env.get_frame());
                    for Bind { name, val } in &bind {
                        let v = val.eval(env)?; // in current frame
                        new_frame
                            .borrow_mut()
                            .insert_new(&name, &v)
                            .context("[let] cannot use the same parameter in let bindings")?;
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
                            .insert_new(&name, &V::new(closure))
                            .context("[named-let] name {} is already used as binding parameter")?;
                    }
                    let Body { defs, exps, ret } = body;
                    for def in defs {
                        def.eval(env)?;
                    }
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = *ret;
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
                }

                Exp::LetRec(bind, Body { defs, exps, ret }) => {
                    let frame = env.get_frame();
                    let new_frame = Frame::empty(&frame);
                    for Bind { name, val: _ } in &bind {
                        new_frame
                            .borrow_mut()
                            .alloc_new(&name)
                            .context("[letrec] cannot use the same variable in bindings")?;
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
                }
                Exp::If {
                    cond,
                    then_exp,
                    else_exp,
                } => {
                    let cond = cond.eval(env)?;
                    let b = match *cond.get().borrow() {
                        SchemeVal::Bool(b) => b,
                        _ => bail!("condition of if statement is not bool"),
                    };
                    if b {
                        exp = *then_exp;
                    } else {
                        match else_exp {
                            None => {
                                env.set_frame(initial_frame);
                                break Ok(V::new(SchemeVal::None));
                            }
                            Some(v) => {
                                exp = *v;
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
                                    return Ok(V::new(SchemeVal::None));
                                }
                            },
                            Some(Branch { cond, then, ret }) => {
                                let cond = cond.eval(env)?;
                                match *cond.get().borrow() {
                                    SchemeVal::Bool(b) => {
                                        if b {
                                            for exp in then {
                                                exp.eval(env)?;
                                            }
                                            break ret.clone();
                                        }
                                    }
                                    _ => bail!("[cond] condition must be boolean"),
                                }
                            }
                        }
                    };
                }
                Exp::And(exps) => match &exps[..] {
                    [] => {
                        env.set_frame(initial_frame);
                        break Ok(V::new(SchemeVal::Bool(true)));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            match *exp.eval(env)?.get().borrow() {
                                SchemeVal::Bool(false) => {
                                    env.set_frame(initial_frame);
                                    return Ok(V::new(SchemeVal::Bool(false)));
                                }
                                _ => continue,
                            }
                        }
                        exp = last.clone();
                    }
                },
                Exp::Or(exps) => match &exps[..] {
                    [] => {
                        env.set_frame(initial_frame);
                        break Ok(V::new(SchemeVal::Bool(false)));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            let v = exp.eval(env)?;
                            match *v.get().borrow() {
                                SchemeVal::Bool(false) => continue,
                                _ => {
                                    env.set_frame(initial_frame);
                                    return Ok(v);
                                }
                            }
                        }
                        exp = last.clone();
                    }
                },
                Exp::Begin(exps) => {
                    let (last, exps) = exps
                        .split_last()
                        .context("begin must at least have one argument")?;
                    for exp in exps {
                        exp.eval(env)?;
                    }
                    exp = last.clone();
                }
            }
        }
    }
}

fn let2_env(binds: &[Bind], env: &mut Env) -> Result<()> {
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
fn params_args(p: Params, args: Vec<V>) -> Result<Vec<(Id, V)>> {
    let Params { prms, other } = p;
    ensure!(prms.len() <= args.len(), "number of argument is incorrect");
    match other {
        Some(id) => {
            let mut ret = vec![];
            let n = args.len();
            for (prm, arg) in prms.iter().zip(args.iter()) {
                ret.push((prm.clone(), arg.clone()));
            }
            ret.push((id, V::from_list(&args[n..], None)));
            return Ok(ret);
        }
        None => {
            ensure!(prms.len() == args.len(), "number of argument is incorrect");
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
            Const::Bool(b) => V::new(SchemeVal::Bool(*b)),
            Const::String(ref s) => V::new(SchemeVal::String(s.to_string())),
            Const::Num(n) => V::new(SchemeVal::Num(*n)),
            Const::Nil => V::new(SchemeVal::Nil),
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
            .with_context(|| format!("could not find value {} \n", self.get()))?;
        Ok(v)
    }
}
impl SExp {
    fn eval(&self, env: &mut Env) -> EResult {
        match self {
            SExp::Const(c) => c.eval(),
            SExp::Id(id) => {
                env.add_sym(id);
                env.get_sym(id)
                    .with_context(|| format!("could not find Symbol {}", id.get()))
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
                Ok(V::from_list(&lst[..], tailv))
            }
        }
    }
}
