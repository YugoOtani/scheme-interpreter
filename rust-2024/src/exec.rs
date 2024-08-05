use anyhow::{bail, Context};
use gc::Gc;
use std::collections::{HashMap, LinkedList};
const STACK_SIZE: usize = 100;
use crate::{
    insn::{ClosureInfo, Insn},
    scheme_value::*,
};

pub struct VM {
    global: HashMap<String, Value>,
    stack: Vec<Value>,

    #[cfg(debug_assertions)]
    stk_addr: *const Value,
}
struct CallFrame {
    base: usize,
    ip_prev: *mut Insn,
    closure: Option<Ptr<ObjClosure>>, //closed_upvalues: Vec<ObjUpvalue>,
}

impl VM {
    pub fn new() -> Self {
        let stack: Vec<Value> = Vec::with_capacity(STACK_SIZE);
        #[cfg(debug_assertions)]
        let stk_addr = stack.as_ptr();
        Self {
            global: HashMap::new(),
            stack, // メモリのリアロケーションが起こらないようにする
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
                print!("{:?}  ", ip.as_ref().unwrap());
                for i in 0..self.stack.len() {
                    if i == cur_frame.base {
                        print!("|");
                    }
                    print!("{} ", self.stack[i].to_string(),)
                }
                println!("");

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
                        self.push(Value::string(s.to_string()))?;
                    }
                    Insn::Cons => {
                        let cdr = self.pop()?;
                        let car = self.pop()?;
                        self.push(Value::cons(car, cdr))?;
                    }
                    Insn::Car => {
                        let lst = self.pop()?;
                        self.push(Value::car(lst.as_cons()?))?;
                    }
                    Insn::Cdr => {
                        let lst = self.pop()?;
                        self.push(Value::cdr(lst.as_cons()?))?;
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
                        let mut upv = vec![];
                        for &(is_local, index) in upv_info {
                            let location = if is_local {
                                ValueRawPtr((&mut self.stack[cur_frame.base + index]) as *mut Value)
                            } else {
                                ValueRawPtr(get_upvalue(index, &cur_frame))
                            };
                            upv.push(ObjUpvalue { location });
                        }
                        self.push(Value::closure(insn.clone(), *arity, upv))?;
                    }
                    Insn::Call(nargs) => {
                        let base = self.stack.len() - nargs - 1;
                        let closure = self.stack[base].as_closure()?;
                        let arity = Value::arity(closure);
                        if arity != *nargs {
                            bail!("expect {arity} argument, found {nargs}")
                        }
                        let mut new_frame = CallFrame {
                            base,
                            ip_prev: ip,
                            closure: Some(Gc::clone(closure)),
                        };
                        (cur_frame, new_frame) = (new_frame, cur_frame);
                        par_frames.push(new_frame);
                        ip = Value::insn_ptr(closure).cast_mut();
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
                        self.push(get_upvalue(*i, &cur_frame).as_ref().unwrap().clone())?;
                    }
                    Insn::SetUpvalue(i) => {
                        *get_upvalue(*i, &cur_frame) = self.pop()?;
                    }
                    Insn::SetCar => {
                        let car = self.pop()?;
                        let cons = self.pop()?;
                        let cons = cons.as_cons()?;
                        Value::set_car(cons, car)
                    }
                    Insn::SetCdr => {
                        let cdr = self.pop()?;
                        let cons = self.pop()?;
                        let cons = cons.as_cons()?;
                        Value::set_cdr(cons, cdr)
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
                    Insn::CloseUpvalue(_) => todo!(),
                }
            }
        }
    }
}
/// if frame == None or
unsafe fn get_upvalue(i: usize, frame: &CallFrame) -> *mut Value {
    let closure: Ptr<ObjClosure> = frame.closure.as_ref().map(|a| Gc::clone(a)).unwrap();
    let upv_obj = &closure.as_ref().borrow().upvalues[i];
    upv_obj.location.0
}
