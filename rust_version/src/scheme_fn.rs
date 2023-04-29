use std::rc::Rc;

use crate::token::SchemeVal as S;
pub fn root_fn() -> Vec<(String, Rc<S>)> {
    vec![sf("+", add), sf("-", sub)]
}
fn sf(s: &str, f: impl Fn(Vec<Rc<S>>) -> Result<S, String> + 'static) -> (String, Rc<S>) {
    (s.to_string(), Rc::new(S::RootFn(Box::new(f))))
}
fn add(args: Vec<Rc<S>>) -> Result<S, String> {
    let mut ans = 0;
    for e in args {
        match e.as_ref() {
            S::Num(n) => ans += n,
            _ => return Err(format!("invalid argument")),
        }
    }
    Ok(S::Num(ans))
}
fn sub(args: Vec<Rc<S>>) -> Result<S, String> {
    match &args[..] {
        [] => Err(format!("[-] number of argument is incorrect")),
        [h, t @ ..] => match h.as_ref() {
            S::Num(i) => {
                let mut ans = *i;
                for e in t {
                    match e.as_ref() {
                        S::Num(j) => ans += *j,
                        _ => return Err(format!("[-] invalid argument")),
                    }
                }
                Ok(S::Num(ans))
            }
            _ => return Err(format!("[-] invalid argument")),
        },
    }
}
