use crate::env::Env;
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
        else_exp: Box<Option<Exp>>,
    },
    Cond {
        branches: Vec<Branch>,
        else_branch: Option<Vec<Exp>>,
    },
    And(Vec<Exp>),
    Or(Vec<Exp>),
    Begin(Vec<Exp>),
}
#[derive(Debug, Clone)]
pub struct Branch {
    pub cond: Exp,
    pub then: Vec<Exp>,
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
        tail: Box<Option<SExp>>,
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
    RootFn(Box<dyn Fn(Vec<Rc<SchemeVal>>) -> Result<SchemeVal, String>>),
    Closure(Rc<RefCell<Env>>, Params, Body),
    None,
}
impl SchemeVal {
    pub fn from_list(v: &[Rc<SchemeVal>]) -> SchemeVal {
        if v.len() == 0 {
            SchemeVal::Nil
        } else {
            SchemeVal::Pair(v[0].clone(), Rc::new(Self::from_list(&v[1..])))
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
            SchemeVal::Pair(v1, v2) => format!("({} {})", v1.to_string(), v2.to_string()),
            SchemeVal::RootFn(_) | SchemeVal::Closure(_, _, _) => "#<procedure>".to_string(),
            SchemeVal::None => "(none)".to_string(),
        }
    }
}
