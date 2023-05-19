use crate::token::*;
use std::cell::*;
use std::rc::*;
pub struct V {}
impl V {
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        todo!()
    }
    pub fn num(n: Num) -> V {
        Self::new(SchemeVal::Num(n))
    }
    pub fn bool(b: bool) -> V {
        Self::new(SchemeVal::Bool(b))
    }
    pub fn string(s: &str) -> V {
        Self::new(SchemeVal::String(s.to_string()))
    }
    pub fn nil() -> V {
        Self::new(SchemeVal::Nil)
    }
    pub fn sym(id: Id) -> V {
        Self::new(SchemeVal::Sym(id))
    }
    pub fn none() -> V {
        Self::new(SchemeVal::None)
    }
    pub fn pair(car: &V, cdr: &V) -> V {
        Self::new(SchemeVal::Pair(car.clone(), cdr.clone()))
    }
    pub fn new(s: SchemeVal) -> V {
        todo!()
    }
    pub fn clone(&self) -> V {
        todo!()
    }
    pub fn get(&self) -> Rc<RefCell<SchemeVal>> {
        todo!()
    }
    pub fn to_list(&self) -> (Vec<V>, Option<V>) {
        fn helper(v: &V, mut acc: Vec<V>) -> (Vec<V>, Option<V>) {
            match &*v.get().borrow() {
                SchemeVal::Pair(h, t) => {
                    acc.push(h.clone());
                    helper(&t, acc)
                }
                SchemeVal::Nil => (acc, None),
                SchemeVal::Lazy(sexp) => helper(&sexp.to_v(), acc),
                _ => (acc, Some(v.clone())),
            }
        }
        helper(self, vec![])
    }
    pub fn to_string(&self) -> String {
        match &*self.get().borrow() {
            SchemeVal::Num(n) => n.to_string(),
            SchemeVal::Bool(b) => {
                if *b {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            SchemeVal::String(s) => format!("\"{s}\""),
            SchemeVal::Nil => "()".to_string(),
            SchemeVal::Sym(id) => id.get().to_string(),
            SchemeVal::Pair(_, _) => {
                // (a (b (c d))) => (a b c . d)
                let (lst, tail) = self.to_list();
                let mut ret = String::from("(");
                ret.push_str(&lst[0].to_string());
                for e in &lst[1..] {
                    ret.push_str(&format!(" {}", e.to_string())[..]);
                }
                let tail = match tail {
                    None => ")".to_string(),
                    Some(t) => format!(" . {})", t.to_string()),
                };
                ret.push_str(&tail[..]);
                ret
            }
            SchemeVal::RootFn(_) | SchemeVal::Closure(_, _, _) => "#<procedure>".to_string(),
            SchemeVal::Macro(_, _) => "#<macro>".to_string(),
            SchemeVal::Lazy(sexp) => sexp.to_v().to_string(),
            SchemeVal::None => "(none)".to_string(),
        }
    }
    pub fn from_list(v: &[V], tail: Option<V>) -> V {
        if v.len() == 0 {
            match tail {
                None => V::new(SchemeVal::Nil),
                Some(v) => v,
            }
        } else {
            let pair = SchemeVal::Pair(v[0].clone(), Self::from_list(&v[1..], tail));
            V::new(pair)
        }
    }
    pub fn is_list(&self) -> bool {
        todo!()
        /*match &*self.get().borrow() {
            SchemeVal::Pair(_, t) => match *t.0.clone().borrow() {
                SchemeVal::Nil => true,
                _ => t.is_list(),
            },
            SchemeVal::Nil => true,
            SchemeVal::Lazy(ref sexp) => match &*sexp.to_v().get().borrow() {
                SchemeVal::Pair(_, cdr) => match *cdr.0.clone().borrow() {
                    SchemeVal::Nil => true,
                    _ => cdr.is_list(),
                },
                SchemeVal::Nil => true,
                _ => unreachable!(),
            },
            _ => false,
        }*/
    }
}
