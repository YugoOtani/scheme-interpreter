use crate::{
    datastructure::SortedSet,
    memory::{Memory, MemoryWithoutGC, Ptr},
    upvalue::{ObjUpvalue, ObjUpvalueRef},
    VERBOSE,
};
use anyhow::{bail, Context};
use std::collections::HashMap;
const STACK_SIZE: usize = 100;
use crate::{
    insn::{ClosureInfo, Insn},
    scheme_value::*,
};

pub struct VM<'a> {
    global: HashMap<String, Value>,
    stack: Vec<Value>,
    upvalues: SortedSet<ObjUpvalueRef>,
    memory: MemoryWithoutGC<'a>,
    #[cfg(debug_assertions)]
    stk_addr: *const Value,
}
struct CallFrame {
    base: usize,
    ip_prev: *mut Insn,
    closure: Option<Ptr<ObjClosure>>, //closed_upvalues: Vec<ObjUpvalue>,
}
impl CallFrame {
    fn get_upvalue_at<'a>(&'a self, i: usize) -> anyhow::Result<ObjUpvalueRef> {
        self.closure
            .as_ref()
            .map(|cls| cls.upvalues[i].clone())
            .context("cannot access upvalue of root frame")
    }
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        let stack: Vec<Value> = Vec::with_capacity(STACK_SIZE);
        #[cfg(debug_assertions)]
        let stk_addr = stack.as_ptr();
        Self {
            global: HashMap::new(),
            stack, // メモリのリアロケーションが起こらないようにする
            upvalues: SortedSet::new(),
            memory: MemoryWithoutGC::new(),
            #[cfg(debug_assertions)]
            stk_addr,
        }
    }
    fn push(&mut self, v: Value) -> anyhow::Result<()> {
        if self.stack.len() >= STACK_SIZE {
            bail!("stack is full")
        } else {
            self.stack.push(v);
            #[cfg(debug_assertions)]
            assert!(self.stack.as_ptr() == self.stk_addr);
            Ok(())
        }
    }
    fn pop(&mut self) -> anyhow::Result<Value> {
        let ret = self.stack.pop().context("stack is empty")?;
        #[cfg(debug_assertions)]
        assert!(self.stack.as_ptr() == self.stk_addr);

        Ok(ret)
    }
    fn stk_truncate(&mut self, len: usize) {
        self.stack.truncate(len);
        #[cfg(debug_assertions)]
        assert!(self.stack.as_ptr() == self.stk_addr);
    }
    pub fn exec(&mut self, mut insn: Vec<Insn>) -> anyhow::Result<()> {
        let mut ip = insn.as_mut_ptr();
        let root = CallFrame {
            ip_prev: ip, // not used
            base: 0,
            closure: None,
            //closed_upvalues: vec![],
        };
        let mut par_frames = vec![];
        let mut cur_frame = root;
        unsafe {
            loop {
                if VERBOSE {
                    print!("{:?}  ", ip.as_ref().unwrap());
                    for i in 0..self.stack.len() {
                        if i == cur_frame.base {
                            print!("|");
                        }
                        print!("{} ", self.stack[i].to_string(),)
                    }
                    println!("")
                }
                let tmp = ip;
                ip = ip.add(1);
                match tmp.as_ref().unwrap() {
                    Insn::None => unreachable!(),
                    Insn::NoneValue => {
                        self.push(Value::None)?;
                    }
                    Insn::Nil => {
                        self.push(Value::Nil)?;
                    }
                    Insn::True => {
                        self.push(Value::True)?;
                    }
                    Insn::False => {
                        self.push(Value::False)?;
                    }
                    Insn::Not => {
                        let t = self.pop()?;
                        let b = t.as_bool()?;
                        if b {
                            self.push(Value::False)?;
                        } else {
                            self.push(Value::True)?;
                        }
                    }
                    Insn::Int(i) => {
                        self.push(Value::Int(*i))?;
                    }
                    Insn::Add => {
                        let v0 = self.pop()?.as_int()?;
                        let v1 = self.pop()?.as_int()?;
                        self.push(Value::Int(v0 + v1))?;
                    }
                    Insn::Sub => {
                        let v0 = self.pop()?.as_int()?;
                        let v1 = self.pop()?.as_int()?;
                        self.push(Value::Int(v0 - v1))?;
                    }
                    Insn::Mul => {
                        let v0 = self.pop()?.as_int()?;
                        let v1 = self.pop()?.as_int()?;
                        self.push(Value::Int(v0 * v1))?;
                    }
                    Insn::PushStr(s) => {
                        let ptr = self.memory.alloc(s.to_string());
                        self.push(Value::String(ptr))?;
                    }
                    Insn::Cons => {
                        let cdr = self.pop()?;
                        let car = self.pop()?;
                        let ptr = self.memory.alloc((car, cdr));
                        self.push(Value::Cons(ptr))?;
                    }
                    Insn::Car => {
                        let lst = self.pop()?;
                        self.push(lst.as_cons()?.0.clone())?;
                    }
                    Insn::Cdr => {
                        let lst = self.pop()?;
                        self.push(lst.as_cons()?.1.clone())?;
                    }
                    Insn::NewGlobal(id) => {
                        let t = self.pop()?;
                        self.global.insert(id.clone(), t);
                    }
                    Insn::SetGlobal(id) => {
                        let t = self.pop()?;
                        match self.global.get_mut(id) {
                            Some(v) => *v = t,
                            None => bail!("variable {} not found", id),
                        }
                    }

                    Insn::GetGlobal(id) => match self.global.get(id) {
                        Some(v) => {
                            self.push(v.clone())?;
                        }
                        None => bail!("variable {} not found", id),
                    },
                    Insn::GetLocal(i) => {
                        let t = self.stack[cur_frame.base + i].clone();
                        self.push(t)?;
                    }
                    Insn::SetLocal(i) => {
                        let t = self.pop()?;
                        self.stack[cur_frame.base + i] = t;
                    }
                    Insn::Print => {
                        let t = self.pop()?;
                        println!("{}", t.to_string())
                    }

                    Insn::Exit => {
                        return Ok(());
                    }
                    Insn::PushClosure(closure) => {
                        let ClosureInfo {
                            arity,
                            insn,
                            upvalues: upv_info,
                        } = &**closure;
                        let mut upvs = vec![];
                        for &(is_local, index) in upv_info {
                            if is_local {
                                let loc = (&self.stack[cur_frame.base + index]) as *const Value;
                                let loc = Ptr::new(loc);
                                let upv = self.capture_upvalue(loc);
                                upvs.push(upv);
                            } else {
                                let upv = cur_frame.get_upvalue_at(index).unwrap();
                                upvs.push(upv);
                            }
                        }
                        let closure = ObjClosure {
                            insn: insn.clone(),
                            arity: *arity,
                            upvalues: upvs,
                        };
                        let closure = Value::Closure(self.memory.alloc(closure));
                        self.push(closure)?;
                    }
                    Insn::Call(nargs) => {
                        let base = self.stack.len() - nargs - 1;
                        let closure = self.stack[base].as_mut_closure()?;
                        let arity = closure.arity;
                        if arity != *nargs {
                            bail!("expect {arity} argument, found {nargs}")
                        }
                        let mut new_frame = CallFrame {
                            base,
                            ip_prev: ip,
                            closure: Some(closure.clone()),
                        };
                        (cur_frame, new_frame) = (new_frame, cur_frame);
                        par_frames.push(new_frame);
                        ip = closure.insn.as_mut_ptr();
                    }
                    Insn::Return => {
                        // f arg1 arg2 ret_val sp
                        let top = self.pop()?;
                        self.stk_truncate(cur_frame.base);
                        self.push(top)?;
                        ip = cur_frame.ip_prev;
                        cur_frame = par_frames.pop().unwrap();
                    }
                    Insn::GetUpvalue(i) => {
                        let upv = cur_frame.get_upvalue_at(*i).unwrap();
                        self.push(upv.get_value())?;
                    }
                    Insn::SetUpvalue(i) => {
                        cur_frame.get_upvalue_at(*i).unwrap().set_value(self.pop()?);
                    }
                    Insn::SetCar => {
                        let car = self.pop()?;
                        let mut cons = self.pop()?;
                        let cons = cons.as_mut_cons()?;
                        cons.0 = car;
                    }
                    Insn::SetCdr => {
                        let cdr = self.pop()?;
                        let mut cons = self.pop()?;
                        let cons = cons.as_mut_cons()?;
                        cons.1 = cdr;
                    }
                    Insn::Pop => {
                        self.pop()?;
                    }
                    Insn::PopN(n) => {
                        let l = self.stack.len();
                        if l < *n {
                            bail!("stack is empty")
                        } else {
                            self.stack.truncate(l - *n);
                        }
                    }
                    Insn::CloseUpvalue(i) => {
                        let v = self.stack[cur_frame.base + i].clone();
                        let loc = &self.stack[cur_frame.base + i] as *const Value;
                        self.close_upvalue(loc, v);
                    }
                }
            }
        }
    }
    fn capture_upvalue(&mut self, location: Ptr<Value>) -> ObjUpvalueRef {
        let sml = self.upvalues.iter().find(|upv| upv.location <= location);
        if sml.is_none() || sml.unwrap().location < location {
            let loc = ObjUpvalueRef(self.memory.alloc(ObjUpvalue { location }));
            self.upvalues.find_or_insert(loc).clone()
        } else {
            sml.unwrap().clone()
        }
    }
    fn close_upvalue(&mut self, loc: *const Value, v: Value) {
        for upv in self.upvalues.iter_mut() {
            if upv.location == Ptr::new(loc) {
                upv.location = self.memory.alloc(v);
                return;
            }
        }
        panic!()
    }
}
