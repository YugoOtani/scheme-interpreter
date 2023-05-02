use crate::frame::Frame;
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

#[derive(Debug, Clone)]
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
        if s.chars().all(|c| Self::is_valid(c)) || s.parse::<i64>().is_ok() {
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
type Num = i64;

pub enum SchemeVal {
    Num(Num),
    Bool(bool),
    String(String),
    Nil,
    Sym(Id),
    Pair(Rc<SchemeVal>, Rc<SchemeVal>),
    RootFn(Box<dyn Fn(Vec<Rc<SchemeVal>>) -> Result<Rc<SchemeVal>, String>>),
    Closure(Rc<RefCell<Frame>>, Params, Body),
    None,
    Undefined,
}
impl SchemeVal {
    pub fn is_list(&self) -> bool {
        match self {
            SchemeVal::Pair(_, t) => match t.as_ref() {
                SchemeVal::Nil => true,
                t => t.is_list(),
            },
            _ => false,
        }
    }
    pub fn to_list(&self) -> (Vec<&SchemeVal>, Option<&SchemeVal>) {
        fn helper<'a>(
            v: &'a SchemeVal,
            mut acc: Vec<&'a SchemeVal>,
        ) -> (Vec<&'a SchemeVal>, Option<&'a SchemeVal>) {
            match v {
                SchemeVal::Pair(h, t) => {
                    acc.push(h.as_ref());
                    helper(t.as_ref(), acc)
                }
                SchemeVal::Nil => (acc, None),
                _ => (acc, Some(v)),
            }
        }
        helper(self, vec![])
    }
    pub fn from_list(v: &[Rc<SchemeVal>], tail: Option<Rc<SchemeVal>>) -> Rc<SchemeVal> {
        if v.len() == 0 {
            match tail {
                None => Rc::new(SchemeVal::Nil),
                Some(v) => v,
            }
        } else {
            Rc::new(SchemeVal::Pair(
                v[0].clone(),
                Self::from_list(&v[1..], tail),
            ))
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            SchemeVal::Num(n) => n.to_string(),
            SchemeVal::Bool(b) => {
                if *b {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            SchemeVal::String(s) => s.to_string(),
            SchemeVal::Nil => "()".to_string(),
            SchemeVal::Sym(id) => id.get().to_string(),
            SchemeVal::Pair(_, _) => {
                // (a (b (c d))) => (a b c . d)
                let (lst, tail) = self.to_list();
                assert!(!lst.is_empty());
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
            SchemeVal::None => "(none)".to_string(),
            SchemeVal::Undefined => panic!(),
        }
    }
}
