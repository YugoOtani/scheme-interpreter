use anyhow::*;

use crate::{insn::Insn, memory::Ptr, upvalue::ObjUpvalueRef};

#[derive(Debug, Clone)]
pub enum Value {
    None,
    Nil,
    Char(char),
    Int(i64),
    True,
    False,
    String(Ptr<String>),
    Cons(Ptr<Cons>),
    Closure(Ptr<ObjClosure>),
}
#[derive(Debug)]
pub struct ObjClosure {
    pub insn: Vec<Insn>,
    pub arity: usize,
    pub upvalues: Vec<ObjUpvalueRef>,
}

pub type Cons = (Value, Value);

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

impl Value {
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::None => "(none)",
            Value::Nil => "nil",
            Value::Char(_) => "char",
            Value::Int(_) => "int",
            Value::True => "bool",
            Value::False => "bool",
            Value::Closure(..) => "closure",
            Value::Cons(..) => "pair",
            Value::String(..) => "string",
        }
    }
    pub fn as_bool(&self) -> anyhow::Result<bool> {
        match self {
            Self::True => Ok(true),
            Self::False => Ok(false),
            _ => bail!("expected bool, found {}", self.type_name()),
        }
    }
    pub fn as_int(&self) -> anyhow::Result<i64> {
        match self {
            Self::Int(i) => Ok(*i),
            _ => bail!("expected int, found {}", self.type_name()),
        }
    }
    pub fn as_char(&self) -> anyhow::Result<char> {
        match self {
            Self::Char(c) => Ok(*c),
            _ => bail!("expected char, found {}", self.type_name()),
        }
    }
    pub fn as_nil(&self) -> anyhow::Result<()> {
        match self {
            Self::Nil => Ok(()),
            _ => bail!("expected nil, found {}", self.type_name()),
        }
    }
    pub fn as_cons(&self) -> anyhow::Result<&Ptr<Cons>> {
        match self {
            Value::Cons(cell) => Ok(cell),
            _ => bail!("expected pair, found {}", self.type_name()),
        }
    }
    pub fn as_mut_cons(&mut self) -> anyhow::Result<&mut Ptr<Cons>> {
        match self {
            Value::Cons(cell) => Ok(cell),
            _ => bail!("expected pair, found {}", self.type_name()),
        }
    }
    pub fn as_closure(&self) -> anyhow::Result<&Ptr<ObjClosure>> {
        match self {
            Value::Closure(obj) => Ok(obj),
            e => bail!("expected pair, found {}", e.type_name()),
        }
    }
    pub fn as_mut_closure(&mut self) -> anyhow::Result<&mut Ptr<ObjClosure>> {
        match self {
            Value::Closure(obj) => Ok(obj),
            e => bail!("expected pair, found {}", e.type_name()),
        }
    }
}
impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::None => "(none)".to_string(),
            Value::Nil => "()".to_string(),
            Value::Char(c) => c.to_string(),
            Value::Int(i) => i.to_string(),
            Value::True => "#t".to_string(),
            Value::False => "#f".to_string(),
            Value::Closure(..) => "#<procedure>".to_string(),
            Value::String(s) => s.to_string(),
            Value::Cons(cell) => {
                let (car, cdr) = (&cell.0, &cell.1);
                format!("({} {})", car.to_string(), cdr.to_string())
            }
        }
    }
}
