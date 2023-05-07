use crate::token::SchemeVal as S;
use crate::{env::Env, token::V};
use anyhow::{bail, Result};
pub fn root_fn() -> Vec<(String, V)> {
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
        sf("set-car!", set_car),
    ]
}
fn sf(s: &str, f: impl Fn(Vec<V>, &mut Env) -> Result<V> + 'static) -> (String, V) {
    (s.to_string(), V::new(S::RootFn(Box::new(f))))
}
fn set_car(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => {
            let cdr = {
                let rf = x.get();
                let rf = rf.borrow();
                match &*rf {
                    S::Pair(_, cdr) => cdr.clone(),
                    _ => bail!("[set-car] first argument must be a pair"),
                }
            };
            *x.get().borrow_mut() = S::Pair(y.clone(), cdr);
            Ok(V::none())
        }
        _ => bail!("[set-car] number of argument is incorrect"),
    }
}
fn add(args: Vec<V>, _: &mut Env) -> Result<V> {
    let mut ans = 0;
    for e in args {
        match *e.get().borrow() {
            S::Num(n) => ans += n,
            _ => bail!("invalid argument"),
        }
    }
    Ok(V::new(S::Num(ans)))
}
fn sub(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [] => bail!("[-] number of argument is incorrect"),
        [h, t @ ..] => match *h.get().borrow() {
            S::Num(i) => {
                let mut ans = i;
                for e in t {
                    match *e.get().borrow() {
                        S::Num(j) => ans -= j,
                        _ => bail!("[-] invalid argument"),
                    }
                }
                Ok(V::new(S::Num(ans)))
            }
            _ => bail!("[-] invalid argument"),
        },
    }
}
fn mul(args: Vec<V>, _: &mut Env) -> Result<V> {
    let mut ans = 1;
    for e in args {
        match *e.get().borrow() {
            S::Num(n) => ans *= n,
            _ => bail!("[*] invalid argument"),
        }
    }
    Ok(V::new(S::Num(ans)))
}

fn car(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match &*h.get().borrow() {
            S::Pair(car, _) => Ok(car.clone()),
            _ => bail!("[car] can only be applied to pair"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn caar(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match &*h.get().borrow() {
            S::Pair(car, _) => match &*car.get().borrow() {
                S::Pair(car, _) => Ok(car.clone()),
                _ => bail!("[caar] can only be applied to pair"),
            },
            _ => bail!("[caar] can only be applied to pair"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn cdar(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match &*h.get().borrow() {
            S::Pair(car, _) => match &*car.get().borrow() {
                S::Pair(_, cdr) => Ok(cdr.clone()),
                _ => bail!("[caar] can only be applied to pair"),
            },
            _ => bail!("[caar] can only be applied to pair"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn cdr(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match &*h.get().borrow() {
            S::Pair(_, cdr) => Ok(cdr.clone()),
            _ => bail!("[cdr] can only be applied to pair"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_null(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match *h.get().borrow() {
            S::Nil => Ok(V::new(S::Bool(true))),
            _ => Ok(V::new(S::Bool(false))),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_pair(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match *h.get().borrow() {
            S::Pair(_, _) => Ok(V::new(S::Bool(true))),
            _ => Ok(V::new(S::Bool(false))),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_sym(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match *h.get().borrow() {
            S::Sym(_) => Ok(V::new(S::Bool(true))),
            _ => Ok(V::new(S::Bool(false))),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_list(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => Ok(V::new(S::Bool(h.is_list()))),
        _ => bail!("number of argument is incorrect"),
    }
}
fn length(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match h.to_list() {
            (_, Some(_)) => bail!("[length] argument is not a list"),
            (lst, None) => Ok(V::new(S::Num(lst.len() as i64))),
        },

        _ => bail!("number of argument is incorrect"),
    }
}
fn memq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, l] => {
            fn helper(a: &V, lst: &V) -> bool {
                match &*lst.get().borrow() {
                    S::Pair(car, cdr) => {
                        if V::ptr_eq(&a, &car) {
                            true
                        } else {
                            helper(a, &cdr)
                        }
                    }
                    S::Nil => false,
                    _ => panic!(),
                }
            }
            if l.is_list() {
                Ok(V::new(S::Bool(helper(x, l))))
            } else {
                bail!("[memq] argument is not a list")
            }
        }

        _ => bail!("number of argument is incorrect"),
    }
}
fn list(args: Vec<V>, _: &mut Env) -> Result<V> {
    fn helper(args: &[V]) -> V {
        match args.split_first() {
            None => V::new(S::Nil),
            Some((h, t)) => V::new(S::Pair(h.clone(), helper(t))),
        }
    }
    Ok(helper(&args))
}
fn last(args: Vec<V>, _: &mut Env) -> Result<V> {
    fn helper(lst: &V) -> V {
        match &*lst.get().borrow() {
            S::Pair(car, cdr) => match *cdr.get().borrow() {
                S::Nil => car.clone(),
                S::Pair(_, _) => helper(&cdr),
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    match &args[..] {
        [h] => {
            if h.is_list() {
                match *h.get().borrow() {
                    S::Nil => bail!("[last] list is empty"),
                    S::Pair(_, _) => Ok(helper(h)),
                    _ => panic!(),
                }
            } else {
                bail!("[last] invalid argument")
            }
        }
        _ => bail!("number of argument is incorrect"),
    }
}
fn append(args: Vec<V>, _: &mut Env) -> Result<V> {
    fn helper(x: &V, y: &V) -> V {
        match &*x.get().borrow() {
            S::Nil => y.clone(),
            S::Pair(car, cdr) => V::new(S::Pair(car.clone(), helper(&cdr, y))),
            _ => panic!(),
        }
    }
    match &args[..] {
        [x, y] => {
            if x.is_list() {
                Ok(helper(x, y))
            } else {
                bail!("[append] first argument must be a list")
            }
        }
        _ => bail!("number of argument is incorrect"),
    }
}

fn math_eq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => match (&*x.get().borrow(), &*y.get().borrow()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(V::new(S::Bool(x == y))),
            _ => bail!("[=] invalid argument"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn math_ls(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => match (&*x.get().borrow(), &*y.get().borrow()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(V::new(S::Bool(x < y))),
            _ => bail!("[<] invalid argument"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn math_gt(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => match (&*x.get().borrow(), &*y.get().borrow()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(V::new(S::Bool(x > y))),
            _ => bail!("[>] invalid argument"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn eq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => Ok(V::new(S::Bool(V::ptr_eq(x, y)))),
        _ => bail!("number of argument is incorrect"),
    }
}
fn cons(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => Ok(V::new(S::Pair(x.clone(), y.clone()))),
        _ => bail!("number of argument is incorrect"),
    }
}
