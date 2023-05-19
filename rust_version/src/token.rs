use crate::env::{Env, Frame};
use crate::gc::V;
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
