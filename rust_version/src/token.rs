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
    Lambda(Arg, Body),
    Func {
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
    If {
        cond: Box<Exp>,
        then_exp: Box<Exp>,
        else_exp: Option<Box<Exp>>,
    },
    Cond {
        branches: Vec<Branch>,
        else_branch: Option<Box<Exp>>,
    },
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
    defs: Vec<Define>,
    exps: Vec<Exp>,
}
#[derive(Debug)]
pub struct Arg {
    args: Vec<Id>,
    other: Option<Id>,
}
#[derive(Debug)]
pub struct Id(String);
impl Id {
    fn is_valid(c: char) -> bool {
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
        if s.chars().all(|c| Self::is_valid(c)) {
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
        tail: Option<Box<SExp>>,
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
