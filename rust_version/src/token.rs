use crate::env::{Env, Frame};
use crate::gc::*;
use anyhow::Result;
use std::cell::*;
use std::rc::Rc;
#[derive(Debug, Clone)]
pub enum Toplevel {
    Exp(Exp),
    Define(Define),
    Load(String),
}
#[derive(Debug, Clone)]
pub enum Define {
    Var(Id, Exp),
    Func(Id, Params, Body),
}
#[derive(Debug, Clone)]
pub struct Params {
    pub prms: Vec<Id>,
    pub other: Option<Id>,
}
#[derive(Debug, Clone)]
pub enum Exp {
    Const(Const),
    Id(Id),
    Lambda(Params, Body),
    FunCall {
        fname: Box<Exp>,
        args: Vec<Exp>,
    },
    Quote(SExp),
    Set(Id, Box<Exp>),
    Let {
        name: Option<Id>,
        bind: Vec<Bind>,
        body: Body,
    },
    Let2(Vec<Bind>, Body),
    LetRec(Vec<Bind>, Body),
    If {
        cond: Box<Exp>,
        then_exp: Box<Exp>,
        else_exp: Option<Box<Exp>>,
    },
    Cond {
        branches: Vec<Branch>,
        else_branch: Option<(Vec<Exp>, Box<Exp>)>,
    },
    And(Vec<Exp>),
    Or(Vec<Exp>),
    Begin(Vec<Exp>),
    DefMacro(Id, Params, Box<Exp>),
    ExpandMacro(Id, Vec<SExp>),
}
#[derive(Debug, Clone)]
pub struct Branch {
    pub cond: Exp,
    pub then: Vec<Exp>,
    pub ret: Exp,
}
#[derive(Debug, Clone)]
pub struct Bind {
    pub name: Id,
    pub val: Exp,
}
#[derive(Debug, Clone)]
pub struct Body {
    pub defs: Vec<Define>,
    pub exps: Vec<Exp>,
    pub ret: Box<Exp>,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Id(String);
impl Id {
    pub fn is_valid(c: char) -> bool {
        match c {
            'a'..='z'
            | 'A'..='Z'
            | '0'..='9'
            | '!'
            | '$'
            | '%'
            | '&'
            | '*'
            | '+'
            | '-'
            | '.'
            | '/'
            | '<'
            | '='
            | '>'
            | '?'
            | '@'
            | '^'
            | '_' => true,
            _ => false,
        }
    }
    pub fn new(s: &str) -> Option<Id> {
        // 数値かどうか検査
        if s.chars().all(|c| Self::is_valid(c)) && s.parse::<i64>().is_err() && s != "." {
            Some(Id(s.to_string()))
        } else {
            None
        }
    }
    pub fn get(&self) -> &str {
        &self.0
    }
}
#[derive(Debug, Clone)]
pub enum SExp {
    Const(Const),
    Id(Id),
    List {
        elems: Vec<SExp>,
        tail: Option<Box<SExp>>,
    },
}
#[derive(Debug, Clone)]
pub enum Const {
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
}
pub type Num = i64;

pub enum SchemeVal {
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
    Sym(Id),
    Pair(V, V),
    RootFn(Func),
    Closure(Rc<RefCell<Frame>>, Params, Body),
    Macro(Params, Exp),
    Lazy(SExp), // TODO: rename
    None,
}
type Func = Box<dyn Fn(Vec<V>, &mut Env) -> Result<V>>;
/*pub struct V(Rc<RefCell<SchemeVal>>);




impl V {
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        Rc::ptr_eq(&this.0, &other.0)
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
        V(Rc::new(RefCell::new(s)))
    }
    pub fn clone(&self) -> V {
        V(self.get())
    }
    pub fn get(&self) -> Rc<RefCell<SchemeVal>> {
        Rc::clone(&self.0)
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
        match &*self.get().borrow() {
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
        }
    }
}
impl Drop for SchemeVal {
    fn drop(&mut self) {
        println!("dropping scval")
    }
}*/
