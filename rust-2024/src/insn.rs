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
    GetUpValue(usize),
    SetUpValue(usize),
    PushClosure(Box<(Vec<Insn>, usize)>),

    Print,
    Exit,
    Return,
}
impl Insn {
    pub fn push_closure(insn: Vec<Insn>, arity: usize) -> Insn {
        Self::PushClosure(Box::new((insn, arity)))
    }
}
