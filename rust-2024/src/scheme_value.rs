use anyhow::*;
use gc::*;
use std::{borrow::Borrow, ops::Deref};

use crate::insn::Insn;

#[derive(Debug, Clone, PartialEq, Eq, Trace, Finalize)]
pub enum Value {
    None,
    Nil,
    Char(char),
    Int(i64),
    True,
    False,
    String(Ptr<String>),
    Cons(Ptr<Cons>),
    Closure(Ptr<Closure>),
}
pub type Ptr<T> = Gc<GcCell<T>>;
pub type Closure = (Vec<Value>, Vec<Insn>, Arity);
pub type Cons = (Value, Value);
pub type Arity = usize;

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}
impl Value {
    fn ptr<T: gc::Trace>(t: T) -> Gc<GcCell<T>> {
        Gc::new(GcCell::new(t))
    }
    pub fn cons(a: Self, b: Self) -> Self {
        Self::Cons(Self::ptr((a, b)))
    }
    pub fn closure(insn: Vec<Insn>, arity: usize) -> Self {
        Self::Closure(Self::ptr((vec![], insn, arity)))
    }
    pub fn string(s: String) -> Self {
        Self::String(Self::ptr(s))
    }
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
    pub fn as_closure(&self) -> anyhow::Result<&Ptr<Closure>> {
        match self {
            Value::Closure(obj) => Ok(obj),
            e => bail!("expected pair, found {}", e.type_name()),
        }
    }
    pub fn insn_ptr(closure: &Ptr<Closure>) -> *const Insn {
        closure.as_ref().borrow().deref().1.as_ptr() as *const Insn
    }
    pub fn arity(closure: &Ptr<Closure>) -> usize {
        closure.as_ref().borrow().2
    }
    pub fn car(cons: &Ptr<Cons>) -> Value {
        cons.as_ref().borrow().deref().0.clone()
    }
    pub fn cdr(cons: &Ptr<Cons>) -> Value {
        cons.as_ref().borrow().deref().1.clone()
    }
    pub fn set_car(cons: &Ptr<Cons>, new_car: Value) {
        cons.borrow_mut().0 = new_car
    }
    pub fn set_cdr(cons: &Ptr<Cons>, new_cdr: Value) {
        cons.borrow_mut().1 = new_cdr
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
            Value::String(s) => s.as_ref().borrow().borrow().to_string(),
            Value::Cons(cell) => {
                let (car, cdr) = &*cell.as_ref().borrow();
                format!("({} {})", car.to_string(), cdr.to_string())
            }
        }
    }
}
