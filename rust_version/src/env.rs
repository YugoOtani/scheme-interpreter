use crate::scheme_fn::root_fn;
use crate::token::SchemeVal;
use std::cell::*;
use std::collections::HashMap;
use std::rc::Rc;
pub struct Env {
    pub vars: HashMap<String, Rc<SchemeVal>>,
    pub par: Option<Rc<RefCell<Env>>>,
}

pub fn root_env() -> Env {
    Env {
        vars: root_fn().into_iter().collect(),
        par: None,
    }
}
impl Env {
    pub fn to_string(&self) -> String {
        let mut s = String::new();
        for (k, v) in &self.vars {
            s.push_str(&format!("{k} : {} || ", v.to_string())[..])
        }
        s.push_str("\n\n");
        match self.par {
            None => s,
            Some(ref par) => {
                s.push_str(&par.clone().borrow().to_string()[..]);
                s
            }
        }
    }
    pub fn find<'a>(&'a self, s: &str) -> Option<Rc<SchemeVal>> {
        match self.vars.get(s) {
            Some(v) => Some(v.clone()),
            None => match self.par {
                Some(ref par) => par.clone().borrow().find(s),
                None => None,
            },
        }
    }
    pub fn insert<'a>(&'a mut self, s: &str, val: &'a Rc<SchemeVal>) {
        self.vars.insert(s.to_string(), Rc::clone(val));
    }
    pub fn replace<'a>(&'a mut self, s: &str, val: &'a Rc<SchemeVal>) -> Option<()> {
        if self.vars.contains_key(s) {
            self.vars.insert(s.to_string(), Rc::clone(val));
            return Some(());
        }
        match self.par {
            Some(ref mut parenv) => parenv.clone().borrow_mut().replace(s, val),
            None => None,
        }
    }
}
