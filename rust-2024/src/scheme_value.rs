use anyhow::*;
use gc::*;
use std::ops::Deref;

use crate::insn::Insn;

#[derive(Debug, Clone, PartialEq, Eq, Trace, Finalize)]
pub enum Value {
    None,
    Nil,
    Char(char),
    Int(i64),
    True,
    False,
    Ptr(Gc<GcCell<Object>>),
}
#[derive(Debug, Clone, PartialEq, Eq, Trace, Finalize)]
pub enum Object {
    Cons(Value, Value),
    Closure(Vec<Value>, Vec<Insn>),
}
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
        let tmp = Object::Cons(a, b);
        Self::Ptr(Self::ptr(tmp))
    }
    pub fn closure(insn: Vec<Insn>) -> Self {
        let tmp = Object::Closure(vec![], insn);
        Self::Ptr(Self::ptr(tmp))
    }
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::None => "(none)",
            Value::Nil => "nil",
            Value::Char(_) => "char",
            Value::Int(_) => "int",
            Value::True => "bool",
            Value::False => "bool",
            Value::Ptr(obj) => match &*obj.as_ref().borrow().deref() {
                Object::Closure(..) => "closure",
                Object::Cons(..) => "pair",
            },
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
    pub fn as_cons(&self) -> anyhow::Result<(Value, Value)> {
        match self {
            Value::Ptr(obj) => match obj.as_ref().borrow().deref() {
                Object::Cons(a, b) => Ok((a.clone(), b.clone())),
                Object::Closure(..) => bail!("expected pair, found closure"),
            },
            _ => bail!("expected pair, found {}", self.type_name()),
        }
    }
    pub fn as_closure(&self) -> anyhow::Result<*mut Insn> {
        match self {
            Value::Ptr(obj) => {
                let tmp = obj.borrow();
                match &*tmp {
                    Object::Closure(_, ref b) => Ok(b.as_ptr().cast_mut()),
                    Object::Cons(..) => bail!("expected closure, found pair"),
                }
            }
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
            Value::Ptr(obj) => match &*obj.as_ref().borrow() {
                Object::Closure(..) => "#<closure>".to_string(),
                Object::Cons(o1, o2) => {
                    format!("({} {})", o1.to_string(), o2.to_string())
                }
            },
        }
    }
}
