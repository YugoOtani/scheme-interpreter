use std::rc::Rc;

use crate::token::SchemeVal as S;
pub fn root_fn() -> Vec<(String, Rc<S>)> {
    vec![sf("+", add), sf("-", sub)]
}
fn sf(s: &str, f: impl Fn(Vec<Rc<S>>) -> Result<S, String> + 'static) -> (String, Rc<S>) {
    (s.to_string(), Rc::new(S::RootFn(Box::new(f))))
}
fn add(args: Vec<Rc<S>>) -> Result<S, String> {
    match &args[..] {
        [x, y] => match (x.as_ref(), y.as_ref()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(S::Num(x + y)),
            _ => Err(format!("invalid argument")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
fn sub(args: Vec<Rc<S>>) -> Result<S, String> {
    match &args[..] {
        [x, y] => match (x.as_ref(), y.as_ref()) {
            (S::Num(ref x), S::Num(ref y)) => Ok(S::Num(x - y)),
            _ => Err(format!("invalid argument")),
        },
        _ => Err(format!("number of argument is incorrect")),
    }
}
