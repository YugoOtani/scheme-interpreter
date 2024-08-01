use anyhow::{bail, Context};
use std::collections::HashMap;

use crate::{insn::Insn, scheme_value::*};

pub struct VM {
    global: HashMap<String, Value>,
}
struct CallFrame {
    base: usize,
    ip_prev: *mut Insn,
}

impl VM {
    pub fn new() -> Self {
        Self {
            global: HashMap::new(),
        }
    }
    pub fn exec(&mut self, mut insn: Vec<Insn>) -> anyhow::Result<String> {
        let mut ip = insn.as_mut_ptr();
        let root = CallFrame {
            ip_prev: ip, // not used
            base: 0,
        };
        let mut frames = vec![root];
        let mut stk = vec![];
        let mut frame = &mut frames[0];
        loop {
            match unsafe { ip.as_ref().unwrap() } {
                Insn::None => unreachable!(),
                Insn::NoneValue => stk.push(Value::None),
                Insn::Nil => stk.push(Value::Nil),
                Insn::True => stk.push(Value::True),
                Insn::False => stk.push(Value::False),
                Insn::Not => {
                    let t = pop(&mut stk)?;
                    let b = t.as_bool()?;
                    if b {
                        stk.push(Value::False)
                    } else {
                        stk.push(Value::True)
                    }
                }
                Insn::Int(i) => stk.push(Value::Int(*i)),
                Insn::Add => {
                    let v0 = pop(&mut stk)?.as_int()?;
                    let v1 = pop(&mut stk)?.as_int()?;
                    stk.push(Value::Int(v0 + v1))
                }
                Insn::Sub => todo!(),
                Insn::Mul => todo!(),

                Insn::Cons => {
                    let cdr = pop(&mut stk)?;
                    let car = pop(&mut stk)?;
                    stk.push(Value::cons(car, cdr));
                }
                Insn::Car => {
                    let lst = pop(&mut stk)?;
                    let (car, _) = lst.as_cons()?;
                    stk.push(car);
                }
                Insn::Cdr => {
                    let lst = pop(&mut stk)?;
                    let (_, cdr) = lst.as_cons()?;
                    stk.push(cdr);
                }
                Insn::NewGlobal(id) => {
                    let t = pop(&mut stk)?;
                    self.global.insert(id.clone(), t);
                }
                Insn::SetGlobal(id) => {
                    let t = pop(&mut stk)?;
                    match self.global.get_mut(id) {
                        Some(v) => *v = t,
                        None => bail!("variable {} not found", id),
                    }
                }

                Insn::GetGlobal(id) => match self.global.get(id) {
                    Some(v) => stk.push(v.clone()),
                    None => bail!("variable {} not found", id),
                },
                Insn::GetLocal(i) => {
                    let t = stk[frame.base + i].clone();
                    stk.push(t)
                }
                Insn::SetLocal(i) => {
                    let t = pop(&mut stk)?;
                    stk[frame.base + i] = t;
                }
                Insn::Print => {
                    let t = pop(&mut stk)?;
                    println!("{}", t.to_string())
                }

                Insn::Exit => {
                    let t = pop(&mut stk)?;
                    return Ok(t.to_string());
                }
                Insn::Halt => return Ok("".to_string()),
                Insn::MkClosure(insn) => {
                    stk.push(Value::closure(insn.clone()));
                }
                Insn::Call(nargs) => {
                    // | f arg1 arg2 .. sp
                    let base = stk.len() - nargs - 1;
                    frame = new_frame(&mut frames, base, ip);
                    ip = stk[base].as_closure()?;
                }
                Insn::Return => {
                    // f arg1 arg2 ret_val sp
                    stk.truncate(frame.base);
                    ip = frame.ip_prev;
                    frame = prev_frame(&mut frames);
                }
            }
            ip = unsafe { ip.add(1) };
        }
    }
}
#[inline]
fn pop(stk: &mut Vec<Value>) -> anyhow::Result<Value> {
    stk.pop().context("stack is empty")
}
#[inline]
fn prev_frame(frames: &mut Vec<CallFrame>) -> &mut CallFrame {
    frames.pop();
    let n = frames.len();
    &mut frames[n - 1]
}
#[inline]
fn new_frame(frames: &mut Vec<CallFrame>, base: usize, old_ip: *mut Insn) -> &mut CallFrame {
    let n = frames.len();
    frames.push(CallFrame {
        base,
        ip_prev: old_ip,
    });
    &mut frames[n]
}
