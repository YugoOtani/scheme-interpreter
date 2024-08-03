use gc::{Finalize, Trace};

#[derive(Debug, Clone, PartialEq, Eq, Trace, Finalize)]
pub enum Insn {
    None,
    NoneValue,
    Nil,
    True,
    False,
    Not,
    Int(i64),
    Add,
    Sub,
    Mul,
    Call(usize),
    Cons,
    Car,
    Cdr,
    SetCar,
    SetCdr,
    PushStr(String),
    NewGlobal(String),
    GetGlobal(String),
    SetGlobal(String),
    GetLocal(usize),
    SetLocal(usize),
    GetUpvalue(usize),
    SetUpvalue(usize),
    PushClosure(Box<ClosureInfo>),

    Print,
    Exit,
    Return,
}
#[derive(Debug, Clone, PartialEq, Eq, Trace, Finalize)]
pub struct ClosureInfo {
    pub insn: Vec<Insn>,
    pub upvalues: Vec<(IsLocal, usize)>,
    pub arity: usize,
}
type IsLocal = bool;
