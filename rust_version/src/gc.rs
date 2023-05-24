use crate::token::*;
use std::cell::*;

const N: usize = 1000_000;
const NULL: Option<VBox> = None;
static mut MEMORY: [Option<VBox>; N] = [NULL; N];
static mut EMPTYADDR: Vec<usize> = vec![];
static mut USEDADDR: Vec<usize> = vec![];

pub fn init() {
    unsafe {
        for mem in MEMORY.as_mut() {
            *mem = NULL;
        }
        USEDADDR = vec![];
        EMPTYADDR = Vec::from_iter(0..N);
    }
}

struct VBox {
    val: SchemeVal,
    cnt: usize,
    check: bool,
}

pub struct V {
    ptr: usize,
    root: Cell<bool>,
}

pub fn num(n: Num) -> V {
    V::alloc(SchemeVal::Num(n))
}
pub fn bool(b: bool) -> V {
    V::alloc(SchemeVal::Bool(b))
}
pub fn string(s: &str) -> V {
    V::alloc(SchemeVal::String(s.to_string()))
}
pub fn nil() -> V {
    V::alloc(SchemeVal::Nil)
}
pub fn sym(id: Id) -> V {
    V::alloc(SchemeVal::Sym(id))
}
pub fn pair(car: &V, cdr: &V) -> V {
    let car = car.clone();
    let cdr = cdr.clone();
    car.force_unroot();
    cdr.force_unroot();
    V::alloc(SchemeVal::Pair(car, cdr)) // return value is rooted
}
pub fn none() -> V {
    V::alloc(SchemeVal::None)
}
pub fn root_fn(f: Func) -> V {
    V::alloc(SchemeVal::RootFn(f))
}
pub fn closure(f: SchemeVal) -> V {
    V::alloc(f)
}
pub fn lazy(v: V) -> V {
    v.force_unroot();
    V::alloc(SchemeVal::Lazy(v))
}
pub fn vmacro(p: Params, exp: Exp) -> V {
    V::alloc(SchemeVal::Macro(p, exp))
}

pub fn mark_and_sweep() {
    unsafe {
        for (i, data) in MEMORY.iter().enumerate() {
            if let Some(data) = data {
                println!("{i} {} ", data.val.dbg())
            }
        }
        for &i in &USEDADDR {
            if MEMORY[i].as_ref().unwrap().cnt > 0 {
                MEMORY[i].as_mut().unwrap().mark();
            }
        }
        let mut save_addr = vec![];
        while let Some(i) = USEDADDR.pop() {
            if MEMORY[i].as_ref().unwrap().check {
                MEMORY[i].as_mut().unwrap().unmark();
                save_addr.push(i);
            } else {
                MEMORY[i] = NULL;
                EMPTYADDR.push(i);
            }
        }
        while let Some(i) = save_addr.pop() {
            USEDADDR.push(i)
        }
    }
}
impl VBox {
    pub fn mark(&mut self) {
        if self.check {
            return;
        }
        self.check = true;
        match self.val {
            SchemeVal::Pair(ref car, ref cdr) => {
                car.mark();
                cdr.mark();
            }
            SchemeVal::Lazy(ref v) => {
                v.mark();
            }
            _ => return,
        }
    }
    pub fn unmark(&mut self) {
        if !self.check {
            return;
        }
        self.check = false;

        match self.val {
            SchemeVal::Pair(ref car, ref cdr) => {
                car.unmark();
                cdr.unmark();
            }
            SchemeVal::Lazy(ref v) => {
                v.unmark();
            }
            _ => return,
        }
    }
}
unsafe impl Send for V {}
impl V {
    fn mark(&self) {
        unsafe { MEMORY[self.ptr].as_mut().unwrap().mark() }
    }
    fn unmark(&self) {
        unsafe { MEMORY[self.ptr].as_mut().unwrap().unmark() }
    }
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.ptr == other.ptr
    }

    pub fn to_list(&self) -> (Vec<V>, Option<V>) {
        fn helper(v: &V, mut acc: Vec<V>) -> (Vec<V>, Option<V>) {
            match v.get_val() {
                SchemeVal::Pair(h, t) => {
                    acc.push(h.clone());
                    helper(&t, acc)
                }
                SchemeVal::Nil => (acc, None),
                SchemeVal::Lazy(sexp) => helper(&sexp, acc),
                _ => (acc, Some(v.clone())),
            }
        }
        helper(self, vec![])
    }
    pub fn to_string(&self) -> String {
        match self.get_val() {
            SchemeVal::Num(n) => n.to_string(),
            SchemeVal::Bool(b) => {
                if *b {
                    "#t".to_string()
                } else {
                    "#f".to_string()
                }
            }
            SchemeVal::String(s) => format!("\"{s}\""),
            SchemeVal::Nil => "()".to_string(),
            SchemeVal::Sym(id) => id.get().to_string(),
            SchemeVal::Pair(_, _) => {
                // (a (b (c d))) => (a b c . d)
                let (lst, tail) = self.to_list();

                let mut ret = String::from("(");
                ret.push_str(&lst[0].to_string());
                for e in &lst[1..] {
                    ret.push_str(&format!(" {}", e.to_string())[..]);
                }
                let tail = match tail {
                    None => ")".to_string(),
                    Some(t) => format!(" . {})", t.to_string()),
                };
                ret.push_str(&tail[..]);
                ret
            }
            SchemeVal::RootFn(_) | SchemeVal::Closure(_, _, _) => "#<procedure>".to_string(),
            SchemeVal::Macro(_, _) => "#<macro>".to_string(),
            SchemeVal::Lazy(sexp) => sexp.to_string(),
            SchemeVal::None => "(none)".to_string(),
        }
    }
    pub fn from_list(v: &[V], tail: Option<V>) -> V {
        if v.len() == 0 {
            match tail {
                None => V::alloc(SchemeVal::Nil),
                Some(v) => v,
            }
        } else {
            let pair = SchemeVal::Pair(v[0].clone(), Self::from_list(&v[1..], tail));
            V::alloc(pair)
        }
    }
    pub fn is_list(&self) -> bool {
        match self.get_val() {
            SchemeVal::Pair(_, t) => match t.get_val() {
                SchemeVal::Nil => true,
                _ => t.is_list(),
            },
            SchemeVal::Nil => true,
            SchemeVal::Lazy(ref sexp) => match sexp.get_val() {
                SchemeVal::Pair(_, cdr) => match cdr.get_val() {
                    SchemeVal::Nil => true,
                    _ => cdr.is_list(),
                },
                SchemeVal::Nil => true,
                _ => unreachable!(),
            },
            _ => false,
        }
    }
}

impl Clone for V {
    fn clone(&self) -> Self {
        self.inc_cnt();

        V {
            ptr: self.ptr,
            root: Cell::new(true),
        }
    }
}

impl V {
    fn alloc(val: SchemeVal) -> V {
        unsafe {
            let addr = match EMPTYADDR.pop() {
                None => panic!("address is full!"),
                Some(addr) => {
                    assert!(addr < N && MEMORY[addr].is_none());
                    USEDADDR.push(addr);
                    addr
                }
            };
            MEMORY[addr] = Some(VBox {
                val,
                cnt: 1,
                check: false,
            });
            V {
                ptr: addr,
                root: Cell::new(true),
            }
        }
    }
    pub fn force_unroot(&self) {
        if self.root.get() {
            self.root.set(false);
            self.dec_cnt();
            self.get_val().unroot();
        }
    }
    fn force_root(&self) {
        if !self.root.get() {
            self.inc_cnt();
            self.root.set(true);
        }
    }
    pub fn is_root(&self) -> bool {
        self.root.get()
    }
    pub fn get_cnt(&self) -> usize {
        unsafe {
            if let None = MEMORY[self.ptr].as_ref() {
                println!("{}", self.ptr);
            }
            MEMORY[self.ptr].as_ref().unwrap().cnt
        }
    }
    pub fn inc_cnt(&self) {
        unsafe { MEMORY[self.ptr].as_mut().unwrap().cnt += 1 }
    }
    pub fn dec_cnt(&self) {
        unsafe { MEMORY[self.ptr].as_mut().unwrap().cnt -= 1 }
    }

    pub fn set_val(&self, val: SchemeVal) {
        val.unroot();
        unsafe {
            MEMORY[self.ptr].as_mut().unwrap().val = val;
        }
        self.force_root()
    }
    pub fn get_val(&self) -> &SchemeVal {
        unsafe { &MEMORY[self.ptr].as_ref().unwrap().val }
    }
}
impl Drop for V {
    fn drop(&mut self) {
        if self.is_root() {
            self.dec_cnt();
        }
        if self.get_cnt() == 0 {
            self.get_val().unroot();
        }
    }
}

impl SchemeVal {
    pub fn unroot(&self) {
        if let SchemeVal::Pair(car, cdr) = self {
            car.force_unroot();
            cdr.force_unroot();
        } else if let SchemeVal::Lazy(v) = self {
            v.force_unroot()
        }
    }
}
pub fn print_mem_usage() {
    unsafe {
        println!(
            "{} values are in use",
            MEMORY.iter().filter(|data| data.is_some()).count()
        )
    }
}
