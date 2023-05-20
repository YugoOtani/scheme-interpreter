use crate::gc::*;
use crate::scheme_fn::root_fn;
use crate::token::Id;
use anyhow::Context;
use anyhow::Result;
use std::cell::*;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Frame {
    pub vars: HashMap<Id, Option<V>>,
    pub par: Option<Rc<RefCell<Frame>>>,
}
pub struct Env {
    frame: Rc<RefCell<Frame>>,
    sym: HashMap<Id, V>,
}
impl Env {
    pub fn new() -> Env {
        Env {
            frame: Rc::new(RefCell::new(Frame::root_frame())),
            sym: HashMap::new(),
        }
    }
    pub fn set_frame(&mut self, frame: Rc<RefCell<Frame>>) {
        self.frame = frame;
    }
    pub fn get_frame(&self) -> Rc<RefCell<Frame>> {
        self.frame.clone()
    }
    pub fn add_sym(&mut self, id: &Id) -> V {
        if !self.sym.contains_key(id) {
            unsafe {
                let v = sym(id.clone());
                self.sym.insert(id.clone(), v.clone());
                v
            }
        } else {
            self.sym.get(id).unwrap().clone()
        }
    }
    pub fn add_sym_s(&mut self, s: &str) -> Result<V> {
        let id = Id::new(s).with_context(|| format!("cannot interpret {s} as id"))?;
        Ok(self.add_sym(&id))
    }
    pub fn get_sym(&self, id: &Id) -> Option<V> {
        self.sym.get(id).map(|rc| rc.clone())
    }
}

impl Frame {
    pub fn empty(par: &Rc<RefCell<Frame>>) -> Rc<RefCell<Frame>> {
        let f = Frame {
            vars: HashMap::new(),
            par: Some(par.clone()),
        };
        Rc::new(RefCell::new(f))
    }
    pub fn root_frame() -> Frame {
        Frame {
            vars: root_fn()
                .into_iter()
                .map(|(s, f)| {
                    let s = Id::new(&s).expect("root_fn must generate valid function name");
                    (s, Some(f))
                })
                .collect(),
            par: None,
        }
    }
    pub fn to_string(&self) -> Result<String, String> {
        let mut s = String::new();
        for (k, v) in &self.vars {
            s.push_str(
                &format!(
                    "{} : {} || ",
                    k.get(),
                    if let Some(v) = v {
                        (*v).to_string()
                    } else {
                        format!("None")
                    }
                )[..],
            )
        }
        s.push_str("\n\n");
        match self.par {
            None => Ok(s),
            Some(ref par) => {
                s.push_str(&par.clone().borrow().to_string()?[..]);
                Ok(s)
            }
        }
    }
    pub fn alloc(&mut self, id: &Id) {
        self.vars.insert(id.clone(), None);
    }
    pub fn alloc_new<'a>(&'a mut self, id: &Id) -> Option<()> {
        if self.vars.contains_key(id) {
            None
        } else {
            self.vars.insert(id.clone(), None);
            Some(())
        }
    }
    pub fn find<'a>(&'a self, id: &Id) -> Option<V> {
        match self.vars.get(id) {
            Some(Some(v)) => Some(v.clone()),
            Some(None) => None,
            None => match self.par {
                Some(ref par) => par.clone().borrow().find(id),
                None => None,
            },
        }
    }
    pub fn insert_new<'a>(&'a mut self, id: &Id, val: &'a V) -> Option<()> {
        if self.vars.contains_key(id) {
            None
        } else {
            self.vars.insert(id.clone(), Some(val.clone()));
            Some(())
        }
    }
    pub fn insert<'a>(&'a mut self, id: &Id, val: &'a V) {
        self.vars.insert(id.clone(), Some(val.clone()));
    }
    pub fn replace<'a>(&'a mut self, id: &Id, val: &'a V) -> Option<()> {
        if self.vars.contains_key(id) {
            self.vars.insert(id.clone(), Some(val.clone()));
            return Some(());
        }
        match self.par {
            Some(ref mut parframe) => parframe.clone().borrow_mut().replace(id, val),
            None => None,
        }
    }
}
