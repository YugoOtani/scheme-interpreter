use crate::env::Env;
use crate::gc::V;
use crate::token::SchemeVal as S;
use anyhow::{bail, Context, Result};
pub fn root_fn() -> Vec<(String, V)> {
    vec![
        sf("print", print),
        sf("+", add),
        sf("-", sub),
        sf("*", mul),
        sf("/", div),
        sf("car", car),
        sf("cdr", cdr),
        sf("null?", is_null),
        sf("=", math_eq),
        sf("caar", caar),
        sf("cdar", cdar),
        sf("eq?", eq),
        sf("<", math_ls),
        sf(">", math_gt),
        sf("<=", math_leq),
        sf(">=", math_gteq),
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
        sf("set-cdr!", set_cdr),
        sf("boolean?", is_bool),
        sf("procedure?", is_proc),
        sf("string-append", string_append),
        sf("symbol->string", sym_string),
        sf("string->symbol", string_sym),
        sf("string->number", string_num),
        sf("number->string", num_string),
        sf("not", not),
        sf("number?", is_number),
        sf("neq?", neq),
        sf("equal?", equal),
    ]
}
fn sf(s: &str, f: impl Fn(Vec<V>, &mut Env) -> Result<V> + 'static) -> (String, V) {
    (s.to_string(), V::new(S::RootFn(Box::new(f))))
}
fn print(args: Vec<V>, _: &mut Env) -> Result<V> {
    let mut s = String::new();
    for arg in args {
        s.push_str(&arg.to_string())
    }
    println!("{}", s);
    Ok(V::none())
}
fn neq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => Ok(V::bool(!V::ptr_eq(x, y))),
        _ => bail!("number of argument is incorrect"),
    }
}
fn equal(args: Vec<V>, _: &mut Env) -> Result<V> {
    fn helper(s1: &V, s2: &V) -> bool {
        match &*s1.get().borrow() {
            S::Bool(b1) => {
                if let S::Bool(b2) = &*s2.get().borrow() {
                    b1 == b2
                } else {
                    false
                }
            }
            S::Num(n1) => {
                if let S::Num(n2) = &*s2.get().borrow() {
                    n1 == n2
                } else {
                    false
                }
            }
            S::Nil => matches!(&*s2.get().borrow(), S::Nil),
            S::None => matches!(&*s2.get().borrow(), S::None),
            S::Pair(car1, cdr1) => {
                if let S::Pair(car2, cdr2) = &*s2.get().borrow() {
                    helper(car1, car2) && helper(cdr1, cdr2)
                } else {
                    false
                }
            }
            S::Sym(id1) => {
                if let S::Sym(id2) = &*s2.get().borrow() {
                    let valeq = id1.get() == id2.get();
                    let refeq = V::ptr_eq(s1, s2);
                    assert!(valeq == refeq);
                    valeq
                } else {
                    false
                }
            }
            S::String(s1) => {
                if let S::String(s2) = &*s2.get().borrow() {
                    s1 == s2
                } else {
                    false
                }
            }
            S::Lazy(_) => {
                panic!()
            }
            S::Closure(..) => V::ptr_eq(s1, s2),
            S::RootFn(..) => V::ptr_eq(s1, s2),
            S::Macro(..) => panic!("macro was returned"),
        }
    }
    match &args[..] {
        [x, y] => Ok(V::bool(helper(x, y))),
        _ => bail!("number of argument is incorrect"),
    }
}
fn string_num(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::String(ref s) => {
                let n = s.parse::<i64>().context("[string->number] parse fail")?;
                Ok(V::num(n))
            }
            _ => bail!("[string->number] invalid argument"),
        },
        _ => bail!("[string->number] number of argument is incorrect"),
    }
}
fn not(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Bool(b) => Ok(V::bool(!b)),
            _ => bail!("[not] invalid argument"),
        },
        _ => bail!("[not] number of argument is incorrect"),
    }
}
fn is_number(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Num(_) => Ok(V::bool(true)),
            _ => Ok(V::bool(false)),
        },
        _ => bail!("[not] number of argument is incorrect"),
    }
}
fn num_string(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Num(ref n) => Ok(V::string(&n.to_string())),
            _ => bail!("[number->string] invalid argument"),
        },
        _ => bail!("[number->string] number of argument is incorrect"),
    }
}
fn string_sym(args: Vec<V>, env: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::String(ref s) => env.add_sym_s(s),
            _ => bail!("[string->symbol] invalid argument"),
        },
        _ => bail!("[string->symbol] number of argument is incorrect"),
    }
}
fn sym_string(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Sym(ref id) => Ok(V::string(id.get())),
            _ => bail!("[symbol->string] invalid argument"),
        },
        _ => bail!("[symbol->string] number of argument is incorrect"),
    }
}
fn string_append(args: Vec<V>, _: &mut Env) -> Result<V> {
    let mut ret = String::new();
    for arg in &args[..] {
        match *arg.get().borrow() {
            S::String(ref s) => ret.push_str(s),
            _ => bail!("invalid argument"),
        }
    }
    Ok(V::string(&ret))
}
fn is_bool(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Bool(_) => Ok(V::bool(true)),
            _ => Ok(V::bool(false)),
        },
        _ => bail!("[boolean?] number of argument is incorrect"),
    }
}
fn is_proc(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x] => match *x.get().borrow() {
            S::Closure(..) => Ok(V::bool(true)),
            S::RootFn(_) => Ok(V::bool(true)),
            _ => Ok(V::bool(false)),
        },
        _ => bail!("[boolean?] number of argument is incorrect"),
    }
}
//fn string_append(args: Vec<V>, _: &mut Env) -> Result<V> {}
fn set_car(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => {
            let cdr = {
                let rf = x.get();
                let rf = rf.borrow();
                match &*rf {
                    S::Pair(_, cdr) => cdr.clone(),
                    S::Lazy(sexp) => match &*sexp.to_v().get().borrow() {
                        S::Pair(_, cdr) => cdr.clone(),
                        S::Nil => bail!("[set-car] cannot apply to nil"),
                        _ => unreachable!(),
                    },
                    _ => bail!("[set-car] first argument must be a pair"),
                }
            };
            *x.get().borrow_mut() = S::Pair(y.clone(), cdr);
            Ok(V::none())
        }
        _ => bail!("[set-car] number of argument is incorrect"),
    }
}
fn set_cdr(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => {
            let car = {
                let rf = x.get();
                let rf = rf.borrow();
                match &*rf {
                    S::Pair(car, _) => car.clone(),
                    S::Lazy(sexp) => match &*sexp.to_v().get().borrow() {
                        S::Pair(car, _) => car.clone(),
                        S::Nil => bail!("[set-cdr] cannot apply to nil"),
                        _ => unreachable!(),
                    },
                    _ => bail!("[set-cdr] first argument must be a pair"),
                }
            };
            *x.get().borrow_mut() = S::Pair(car, y.clone());
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
fn div(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [] => bail!("[/] number of argument is incorrect"),
        [h, t @ ..] => match *h.get().borrow() {
            S::Num(i) => {
                let mut ans = i;
                for e in t {
                    match *e.get().borrow() {
                        S::Num(0) => bail!("[/] division by zero"),
                        S::Num(i) => ans /= i,
                        _ => bail!("[/] invalid argument"),
                    }
                }
                Ok(V::new(S::Num(ans)))
            }
            _ => bail!("[/] invalid argument"),
        },
    }
}

fn car(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match &*h.get().borrow() {
            S::Pair(car, _) => Ok(car.clone()),
            S::Lazy(sexp) => match &*sexp.to_v().get().borrow() {
                S::Pair(car, _) => Ok(car.clone()),
                S::Nil => bail!("[car] cannot apply to nil"),
                _ => unreachable!(),
            },
            _ => bail!(
                "[car] can only be applied to pair : given {}",
                h.to_string()
            ),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
// not yet implemented S::Lazy
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
            S::Lazy(sexp) => match &*sexp.to_v().get().borrow() {
                S::Pair(_, cdr) => Ok(cdr.clone()),
                S::Nil => bail!("[cdsr] cannot apply to nil"),
                _ => unreachable!(),
            },
            _ => bail!("[cdr] can only be applied to pair"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_null(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match *h.get().borrow() {
            S::Nil => Ok(V::new(S::Bool(true))),
            S::Lazy(ref sexp) => match &*sexp.to_v().get().borrow() {
                S::Pair(..) => Ok(V::bool(false)),
                S::Nil => Ok(V::bool(true)),
                _ => unreachable!(),
            },
            _ => Ok(V::new(S::Bool(false))),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn is_pair(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [h] => match *h.get().borrow() {
            S::Pair(_, _) => Ok(V::new(S::Bool(true))),
            S::Lazy(ref sexp) => match &*sexp.to_v().get().borrow() {
                S::Pair(..) => Ok(V::bool(true)),
                S::Nil => Ok(V::bool(false)),
                _ => unreachable!(),
            },
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
                    S::Lazy(..) => V::ptr_eq(a, lst),
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
                    S::Lazy(ref sexp) => match &*sexp.to_v().get().borrow() {
                        S::Nil => bail!("[last] list is empty"),
                        _ => Ok(helper(&sexp.to_v())),
                    },
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
            S::Lazy(sexp) => helper(&sexp.to_v(), y),
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
fn math_leq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => match (&*x.get().borrow(), &*y.get().borrow()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(V::new(S::Bool(x <= y))),
            _ => bail!("[<] invalid argument"),
        },
        _ => bail!("number of argument is incorrect"),
    }
}
fn math_gteq(args: Vec<V>, _: &mut Env) -> Result<V> {
    match &args[..] {
        [x, y] => match (&*x.get().borrow(), &*y.get().borrow()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(V::new(S::Bool(x >= y))),
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
