use std::rc::Rc;

use crate::token::SchemeVal as S;
pub fn root_fn() -> Vec<(String, Rc<S>)> {
    vec![
        sf("+", add),
        sf("-", sub),
        sf("*", mul),
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
        sf("length", length),
        sf("memq", memq),
        sf("symbol?", is_sym),
        sf("pair?", is_pair),
        sf("list?", is_list),
        sf("list", list),
        sf("last", last),
        sf("append", append),
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
fn mul(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    let mut ans = 1;
    for e in args {
        match e.as_ref() {
            S::Num(n) => ans *= n,
            _ => return Err(format!("[*] invalid argument")),
        }
    }
    Ok(Rc::new(S::Num(ans)))
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
fn is_pair(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Pair(_, _) => Ok(Rc::new(S::Bool(true))),
            _ => Ok(Rc::new(S::Bool(false))),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn is_sym(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref() {
            S::Sym(_) => Ok(Rc::new(S::Bool(true))),
            _ => Ok(Rc::new(S::Bool(false))),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn is_list(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => Ok(Rc::new(S::Bool(h.as_ref().is_list()))),
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn length(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [h] => match h.as_ref().to_list() {
            (_, Some(_)) => Err(format!("[length] argument is not a list")),
            (lst, None) => Ok(Rc::new(S::Num(lst.len() as i64))),
        },

        _ => Err(format!("number of argument is incorrect")),
    }
}
fn memq(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    match &args[..] {
        [x, l] => {
            fn helper(a: &Rc<S>, lst: &Rc<S>) -> bool {
                match lst.as_ref() {
                    S::Pair(car, cdr) => {
                        if Rc::ptr_eq(a, car) {
                            true
                        } else {
                            helper(a, cdr)
                        }
                    }
                    S::Nil => false,
                    _ => panic!(),
                }
            }
            if l.as_ref().is_list() {
                Ok(Rc::new(S::Bool(helper(x, l))))
            } else {
                Err(format!("[memq] argument is not a list"))
            }
        }

        _ => Err(format!("number of argument is incorrect")),
    }
}
fn list(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    fn helper(args: &[Rc<S>]) -> Rc<S> {
        match args.split_first() {
            None => Rc::new(S::Nil),
            Some((h, t)) => Rc::new(S::Pair(h.clone(), helper(t))),
        }
    }
    Ok(helper(&args))
}
fn last(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    fn helper(lst: &Rc<S>) -> Rc<S> {
        match lst.as_ref() {
            S::Pair(car, cdr) => match cdr.as_ref() {
                S::Nil => car.clone(),
                S::Pair(_, _) => helper(cdr),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    match &args[..] {
        [h] => {
            if h.is_list() {
                match h.as_ref() {
                    S::Nil => Err(format!("[last] list is empty")),
                    S::Pair(_, _) => Ok(helper(h)),
                    _ => panic!(),
                }
            } else {
                Err(format!("[last] invalid argument"))
            }
        }
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn append(args: Vec<Rc<S>>) -> Result<Rc<S>, String> {
    fn helper(x: &Rc<S>, y: &Rc<S>) -> Rc<S> {
        match x.as_ref() {
            S::Nil => y.clone(),
            S::Pair(car, cdr) => Rc::new(S::Pair(car.clone(), helper(cdr, y))),
            _ => panic!(),
        }
    }
    match &args[..] {
        [x, y] => {
            if x.is_list() {
                Ok(helper(x, y))
            } else {
                Err(format!("[append] first argument must be a list"))
            }
        }
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
