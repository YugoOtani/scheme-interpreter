use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

use crate::{
    scheme_value::{Cons, ObjClosure, Value},
    upvalue::ObjUpvalue,
};

pub trait Memory<'a> {
    fn alloc<T: Trace + 'a>(&mut self, t: T) -> Ptr<T>;
}
pub trait Trace {
    fn mark(&mut self);
    fn unmark(&mut self);
}

pub struct Ptr<T>(*mut T);

pub struct MemoryWithoutGC<'a> {
    mem: Vec<Box<dyn Trace + 'a>>,
}
impl<'a> MemoryWithoutGC<'a> {
    pub fn new() -> Self {
        Self { mem: vec![] }
    }
}
impl<'a> Memory<'a> for MemoryWithoutGC<'a> {
    fn alloc<T: Trace + 'a>(&mut self, t: T) -> Ptr<T> {
        let mut p = Box::new(t);
        let ptr = p.as_mut() as *mut T;
        self.mem.push(p);
        Ptr(ptr)
    }
}

impl<T> Ptr<T> {
    /// pのlifetimeは手動で管理する必要がある
    pub fn new(p: *const T) -> Self {
        Self(p.cast_mut())
    }
}
impl<T> Deref for Ptr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref().unwrap() }
    }
}
impl<T> DerefMut for Ptr<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut().unwrap() }
    }
}
impl<T> Clone for Ptr<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<T> PartialEq for Ptr<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.0.eq(&rhs.0)
    }
}
impl<T> Eq for Ptr<T> {}
impl<T> PartialOrd for Ptr<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}
impl<T> Ord for Ptr<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
impl<T: Debug> Debug for Ptr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&[{:?}]", &self.deref())
    }
}

impl Trace for Value {
    fn mark(&mut self) {}
    fn unmark(&mut self) {}
}
impl Trace for String {
    fn mark(&mut self) {}
    fn unmark(&mut self) {}
}
impl Trace for Cons {
    fn mark(&mut self) {}
    fn unmark(&mut self) {}
}
impl Trace for ObjClosure {
    fn mark(&mut self) {}
    fn unmark(&mut self) {}
}
impl Trace for ObjUpvalue {
    fn mark(&mut self) {}
    fn unmark(&mut self) {}
}
