use crate::eval::Env;
#[derive(Debug)]
pub enum Toplevel {
    Exp(Exp),
    Define(Define),
    Load(String),
}
#[derive(Debug)]
pub enum Define {
    Var(Id, Exp),
    Func(Id, Params, Body),
}
#[derive(Debug)]
pub struct Params {
    pub prms: Vec<Id>,
    pub other: Option<Id>,
}
#[derive(Debug)]
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
#[derive(Debug)]
pub struct Branch {
    pub cond: Exp,
    pub then: Vec<Exp>,
}
#[derive(Debug)]
pub struct Bind {
    pub name: Id,
    pub val: Exp,
}
#[derive(Debug)]
pub struct Body {
    pub defs: Vec<Define>,
    pub exps: Vec<Exp>,
}

#[derive(Debug)]
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
#[derive(Debug)]
pub enum SExp {
    Const(Const),
    Id(Id),
    List {
        elems: Vec<SExp>,
        tail: Box<Option<SExp>>,
    },
}
#[derive(Debug)]
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
    Pair(Box<SchemeVal>, Box<SchemeVal>),
    RootFn(Box<dyn Fn(Vec<SchemeVal>) -> SchemeVal>),
    Closure(Env, Params, Body),
    None,
}
