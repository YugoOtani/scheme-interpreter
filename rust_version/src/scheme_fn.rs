use std::rc::Rc;

use crate::token::SchemeVal as S;
pub fn root_fn() -> Vec<(String, Rc<S>)> {
    vec![
        sf("+", add),
        sf("-", sub),
        sf("car", car),
        sf("cdr", cdr),
        sf("null?", is_null),
        sf("=", math_eq),
        sf("caar", caar),
        sf("cdar", cdar),
        sf("eq?", eq),
        sf("<", math_ls),
        sf(">", math_gt),
        sf("cons", cons),
    ]
}
fn sf(s: &str, f: impl Fn(Vec<Rc<S>>) -> Result<Rc<S>, String> + 'static) -> (String, Rc<S>) {
    (s.to_string(), Rc::new(S::RootFn(Box::new(f))))
}
fn add(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    let mut ans = 0;
    for e in args {
        match e.as_ref() {
            S::Num(n) => ans += n,
            _ => return Err(format!("invalid argument")),
        }
    }
    Ok(Rc::new(S::Num(ans)))
}
fn sub(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [] => Err(format!("[-] number of argument is incorrect")),
        [h, t @ ..] => match h.as_ref() {
            S::Num(i) => {
                let mut ans = *i;
                for e in t {
                    match e.as_ref() {
                        S::Num(j) => ans -= *j,
                        _ => return Err(format!("[-] invalid argument")),
                    }
                }
                Ok(Rc::new(S::Num(ans)))
            }
            _ => return Err(format!("[-] invalid argument")),
        },
    }
}
fn car(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Pair(car, _) => Ok(car.clone()),
            _ => Err(format!("[car] can only be applied to pair")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn caar(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Pair(car, _) => match car.as_ref() {
                S::Pair(car, _) => Ok(car.clone()),
                _ => Err(format!("[caar] can only be applied to pair")),
            },
            _ => Err(format!("[caar] can only be applied to pair")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn cdar(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Pair(car, _) => match car.as_ref() {
                S::Pair(_, cdr) => Ok(cdr.clone()),
                _ => Err(format!("[caar] can only be applied to pair")),
            },
            _ => Err(format!("[caar] can only be applied to pair")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn cdr(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Pair(_, cdr) => Ok(cdr.clone()),
            _ => Err(format!("[cdr] can only be applied to pair")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn is_null(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Nil => Ok(Rc::new(S::Bool(true))),
            _ => Ok(Rc::new(S::Bool(false))),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}

fn math_eq(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, y] => match (x.as_ref(), y.as_ref()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(Rc::new(S::Bool(x == y))),
            _ => Err(format!("[=] invalid argument")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn math_ls(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, y] => match (x.as_ref(), y.as_ref()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(Rc::new(S::Bool(x < y))),
            _ => Err(format!("[<] invalid argument")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn math_gt(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, y] => match (x.as_ref(), y.as_ref()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(Rc::new(S::Bool(x > y))),
            _ => Err(format!("[>] invalid argument")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn eq(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, y] => Ok(Rc::new(S::Bool(Rc::ptr_eq(x, y)))),
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn cons(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, y] => Ok(Rc::new(S::Pair(x.clone(), y.clone()))),
        _ => Err(format!("number of argument is incorrect")),
    }
}
