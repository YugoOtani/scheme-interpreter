use crate::{memory::Ptr, scheme_value::Value};
use std::fmt::Debug;
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ObjUpvalueRef(pub Ptr<ObjUpvalue>);
#[derive(Debug)]
pub struct ObjUpvalue {
    pub location: Ptr<Value>,
}

impl ObjUpvalueRef {
    pub unsafe fn get_value(&self) -> Value {
        self.0.location.deref().clone()
    }
    pub unsafe fn set_value(&mut self, v: Value) {
        *self.0.location = v
    }
}

impl DerefMut for ObjUpvalueRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.deref_mut()
    }
}
impl Deref for ObjUpvalueRef {
    type Target = ObjUpvalue;
    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}
