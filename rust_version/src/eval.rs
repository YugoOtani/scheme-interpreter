use crate::dbg_token::TDbg;
use crate::gc::*;
use crate::{env::*, parser::parse_token, token::*, tosexp::ToSExp};
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
                Ok(none())
            }
            Define::Func(id, param, body) => {
                let c = SchemeVal::Closure(env.get_frame(), param.clone(), body.clone());
                env.get_frame().borrow_mut().insert(id, &closure(c));
                Ok(none())
            }
        }
    }
}
impl Exp {
    pub fn eval(&self, env: &mut Env) -> EResult {
        let mut exp = self.clone();
        let initial_frame = env.get_frame();
        let r = loop {
            // loop for tail recursion
            match exp {
                Exp::Const(c) => {
                    env.set_frame(initial_frame);
                    let v = c.eval();
                    break Ok(v);
                }
                Exp::Id(id) => {
                    let v = id.eval(env)?;
                    //println!("{} {}", id.get(), v.to_string());
                    env.set_frame(initial_frame);
                    break Ok(v);
                }
                Exp::FunCall {
                    ref fname,
                    ref args,
                } => {
                    let func = fname.eval(env)?;
                    match func.get_val() {
                        SchemeVal::RootFn(f) => {
                            let mut args_v = vec![];
                            for e in args {
                                let v = e.eval(env)?;
                                args_v.push(v);
                            }
                            env.set_frame(initial_frame);

                            let r = f(args_v, env);

                            break r;
                        }
                        SchemeVal::Closure(parframe, params, Body { defs, exps, ret }) => {
                            let mut args_v = vec![];
                            for e in args {
                                let v = e.eval(env)?;
                                args_v.push(v);
                            }
                            let p_a = params_args(params, &args_v)?;
                            let eval_frame = Frame::empty(&parframe);
                            for (id, val) in p_a {
                                eval_frame
                                    .borrow_mut()
                                    .insert_new(id, &val)
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
                        SchemeVal::Macro(prms, sexp) => {
                            let exps: Vec<SExp> = args.iter().map(|exp| exp.to_sexp()).collect();
                            let res = expand_macro(prms, &exps, sexp, env)?;
                            println!("macro expanded : {}", res.to_string());
                            let top = parse_token(&res.to_string())?;

                            let ret = top.eval(env);
                            env.set_frame(initial_frame);
                            break ret;
                        }

                        _ => bail!("result of {:?} is not a function", fname),
                    }
                }
                Exp::ExpandMacro(ref id, ref exps) => match env.get_frame().borrow().find(&id) {
                    None => bail!("macro {} not found", id.get()),
                    Some(m) => {
                        if let SchemeVal::Macro(prms, sexp) = m.get_val() {
                            let res = expand_macro(prms, &exps, sexp, env)?;
                            let top = parse_token(&res)?;
                            let ret = top.eval(env);
                            env.set_frame(initial_frame);
                            break ret;
                        } else {
                            bail!("{} is not a macro", id.get())
                        }
                    }
                },
                Exp::DefMacro(id, prms, exp) => {
                    env.get_frame()
                        .borrow_mut()
                        .insert(&id, &vmacro(prms, *exp));
                    env.set_frame(initial_frame);
                    break Ok(none());
                }
                Exp::Lambda(prms, body) => {
                    let c = SchemeVal::Closure(env.get_frame(), prms.clone(), body.clone());
                    env.set_frame(initial_frame);
                    break Ok(closure(c));
                }
                Exp::Set(id, exp) => {
                    let v = exp.eval(env)?;
                    env.get_frame()
                        .borrow_mut()
                        .replace(&id, &v)
                        .with_context(|| format!("could not find value {}", id.get()))?;
                    env.set_frame(initial_frame);
                    break Ok(none());
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
                        let c = SchemeVal::Closure(
                            env.get_frame(),
                            Params { prms, other: None },
                            body.clone(),
                        );
                        env.get_frame()
                            .borrow_mut()
                            .insert_new(&name, &closure(c))
                            .context("[named-let] name is already used as binding parameter")?;
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
                    let b = match cond.get_val() {
                        SchemeVal::Bool(b) => *b,
                        _ => bail!("condition of if statement is not bool"),
                    };
                    if b {
                        exp = *then_exp;
                    } else {
                        match else_exp {
                            None => {
                                env.set_frame(initial_frame);
                                break Ok(none());
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
                                    return Ok(none());
                                }
                            },
                            Some(Branch { cond, then, ret }) => {
                                let cond = cond.eval(env)?;
                                match cond.get_val() {
                                    SchemeVal::Bool(b) => {
                                        if *b {
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
                        break Ok(bool(true));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            match exp.eval(env)?.get_val() {
                                SchemeVal::Bool(false) => {
                                    env.set_frame(initial_frame);
                                    return Ok(bool(false));
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
                        break Ok(bool(false));
                    }
                    v => {
                        let (last, elems) = v.split_last().unwrap();
                        for exp in elems {
                            let v = exp.eval(env)?;
                            match v.get_val() {
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
                Exp::Do {
                    ref binds,
                    ref pred,
                    ref ret,
                    ref body,
                } => {
                    let current_frame = env.get_frame();
                    let eval_frame = Frame::empty(&current_frame);
                    for DoBind {
                        name,
                        init,
                        update: _,
                    } in binds
                    {
                        let v = init.eval(env)?;
                        eval_frame.borrow_mut().insert_new(&name, &v);
                    }
                    env.set_frame(eval_frame);
                    break loop {
                        match pred.eval(env)?.get_val() {
                            SchemeVal::Bool(true) => {
                                if ret.is_empty() {
                                    env.set_frame(initial_frame);
                                    break Ok(none());
                                } else {
                                    let (last, other) = ret.split_last().unwrap();
                                    for exp in other {
                                        exp.eval(env)?;
                                    }
                                    let ret = last.eval(env);
                                    env.set_frame(initial_frame);
                                    break ret;
                                }
                            }
                            SchemeVal::Bool(false) => {
                                let Body { defs, exps, ret } = body;
                                for def in defs {
                                    def.eval(env)?;
                                }
                                for exp in exps {
                                    exp.eval(env)?;
                                }
                                ret.eval(env)?;
                                let mut new_vals = vec![];
                                for DoBind {
                                    name: _,
                                    init: _,
                                    update,
                                } in binds
                                {
                                    new_vals.push(update.eval(env)?);
                                }
                                for (name, val) in binds
                                    .iter()
                                    .map(
                                        |DoBind {
                                             name,
                                             init: _,
                                             update: _,
                                         }| name,
                                    )
                                    .zip(new_vals)
                                {
                                    env.get_frame().borrow_mut().replace(name, &val);
                                }
                                continue;
                            }
                            _ => bail!("[do] test result is not bool"),
                        }
                    };
                }
            }
        }?;
        Ok(r)
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
fn expand_macro(prms: &Params, exps: &Vec<SExp>, ret: &Exp, env: &mut Env) -> Result<String> {
    let Params { prms, other } = prms;
    let initial_frame = env.get_frame();
    match other {
        None => {
            if prms.len() != exps.len() {
                bail!("[expand-macro] number of argument is incorrect")
            } else {
                let new_frame = Frame::empty(&initial_frame);
                for (id, exp) in prms.iter().zip(exps.iter()) {
                    new_frame.borrow_mut().insert(id, &lazy(exp.to_v()))
                }
                env.set_frame(new_frame);
                let ret = ret.eval(env)?.to_string();
                env.set_frame(initial_frame);
                Ok(ret)
            }
        }
        Some(v) => {
            if prms.len() > exps.len() {
                bail!("[expand-macro] number of argument is incorrect")
            } else {
                let new_frame = Frame::empty(&initial_frame);

                for (id, exp) in prms.iter().zip(exps.iter()) {
                    let l = lazy(exp.to_v());
                    new_frame.borrow_mut().insert(id, &l)
                }
                let rest = lazy(SExp::list(exps[prms.len()..].to_vec()).to_v());
                new_frame.borrow_mut().insert(v, &rest);
                env.set_frame(new_frame);
                let ret = ret.eval(env)?.to_string();
                env.set_frame(initial_frame);
                Ok(ret)
            }
        }
    }
}
fn params_args<'a>(p: &'a Params, args: &Vec<V>) -> Result<Vec<(&'a Id, V)>> {
    let Params { prms, other } = p;
    ensure!(prms.len() <= args.len(), "number of argument is incorrect");
    match other {
        Some(id) => {
            let mut ret = vec![];
            let n = prms.len();
            for (prm, arg) in prms.iter().zip(args.iter()) {
                ret.push((prm, arg.clone()));
            }
            ret.push((id, V::from_list(&args[n..], None)));
            return Ok(ret);
        }
        None => {
            ensure!(prms.len() == args.len(), "number of argument is incorrect");
            let mut ret = vec![];
            for (prm, arg) in prms.iter().zip(args.iter()) {
                ret.push((prm, arg.clone()));
            }
            return Ok(ret);
        }
    }
}

impl Const {
    fn eval(&self) -> V {
        match self {
            Const::Bool(b) => bool(*b),
            Const::String(ref s) => string(&s.to_string()),
            Const::Num(n) => num(*n),
            Const::Nil => nil(),
        }
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
    pub fn to_v(&self) -> V {
        fn helper(v: &[SExp], tail: &Option<Box<SExp>>) -> V {
            match v {
                [] => panic!(),
                [h] => pair(
                    &h.to_v(),
                    &match tail {
                        Some(v) => v.to_v(),
                        None => nil(),
                    },
                ),
                [h, t @ ..] => pair(&h.to_v(), &helper(t, tail)),
            }
        }

        let v = match self {
            SExp::Const(c) => c.eval(),
            SExp::Id(id) => sym(id.clone()),
            SExp::List { elems, tail } => match &elems[..] {
                [] => nil(),
                e => helper(e, tail),
            },
        };
        v
    }
    fn eval(&self, env: &mut Env) -> EResult {
        match self {
            SExp::Const(c) => Ok(c.eval()),
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
