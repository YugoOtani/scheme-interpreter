use crate::token::*;
use std::cell::*;
use std::ptr::drop_in_place;

//const N: usize = 1000_000;
static mut MEMORY: Vec<V> = vec![];

pub fn num(n: Num) -> V {
    alloc(SchemeVal::Num(n))
}
pub fn bool(b: bool) -> V {
    alloc(SchemeVal::Bool(b))
}
pub fn string(s: &str) -> V {
    alloc(SchemeVal::String(s.to_string()))
}
pub fn nil() -> V {
    alloc(SchemeVal::Nil)
}
pub fn sym(id: Id) -> V {
    alloc(SchemeVal::Sym(id))
}
pub fn pair(car: &V, cdr: &V) -> V {
    let car = car.clone();
    let cdr = cdr.clone();
    car.force_unroot();
    cdr.force_unroot();
    alloc(SchemeVal::Pair(car, cdr)) // return value is rooted
}
pub fn none() -> V {
    alloc(SchemeVal::None)
}
pub fn root_fn(f: Func) -> V {
    alloc(SchemeVal::RootFn(f))
}
pub fn closure(f: SchemeVal) -> V {
    alloc(f)
}
pub fn lazy(v: V) -> V {
    v.force_unroot();
    alloc(SchemeVal::Lazy(v))
}
pub fn vmacro(p: Params, exp: Exp) -> V {
    alloc(SchemeVal::Macro(p, exp))
}
fn alloc(val: SchemeVal) -> V {
    unsafe {
        let v = V::new(val);

        let v1 = v.clone();
        v1.force_unroot();
        MEMORY.push(v1);
        mark_and_sweep();
        assert!(v.deref().cnt == 1);
        v
    }
}
pub fn print_mem_usage() {
    unsafe {
        /*for x in &GC.memory {
            println!("{} {}", x.get().dbg(), x.inner().cnt)
        }*/
        println!("{} values are in use", MEMORY.len())
    }
}

pub fn mark_and_sweep() {
    unsafe {
        for data in &MEMORY {
            if data.deref().cnt > 0 {
                data.mark();
            }
        }
        MEMORY.retain(|data| {
            if data.deref().check {
                data.unmark();
                true
            } else {
                data.free();
                false
            }
        });
    }
}

unsafe impl Send for V {}
impl V {
    pub fn mark(&self) {
        if self.deref().check {
            return;
        }
        self.deref().check = true;
        match self.deref().val {
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
    pub fn unmark(&self) {
        let checked = self.deref().check;
        if !checked {
            return;
        }
        self.deref().check = false;

        match self.deref().val {
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

    fn free(&self) {
        unsafe { drop_in_place(self.ptr) }
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
                None => V::new(SchemeVal::Nil),
                Some(v) => v,
            }
        } else {
            let pair = SchemeVal::Pair(v[0].clone(), Self::from_list(&v[1..], tail));
            V::new(pair)
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

pub struct VBox {
    check: bool,
    val: SchemeVal,
    cnt: usize,
}

pub struct V {
    ptr: *mut VBox,
    root: Cell<bool>,
}
impl Clone for V {
    fn clone(&self) -> Self {
        self.deref().cnt += 1;

        V {
            ptr: self.ptr,
            root: Cell::new(true),
        }
    }
}

impl V {
    fn new(val: SchemeVal) -> V {
        V {
            ptr: Box::into_raw(Box::new(VBox {
                check: false,
                val,
                cnt: 1,
            })),
            root: Cell::new(true),
        }
    }
    fn deref(&self) -> &mut VBox {
        unsafe { self.ptr.as_mut().unwrap() }
    }
    pub fn force_unroot(&self) {
        if self.root.get() {
            self.root.set(false);
            self.deref().cnt -= 1;
            self.deref().val.unroot();
        }
    }
    fn force_root(&self) {
        if !self.root.get() {
            self.deref().cnt += 1;
            self.root.set(true);
        }
    }

    pub fn set_val(&self, val: SchemeVal) {
        val.unroot();
        self.deref().val = val;
        self.force_root()
    }
    pub fn get_val(&self) -> &SchemeVal {
        &self.deref().val
    }
}
impl Drop for V {
    fn drop(&mut self) {
        if self.root.get() {
            self.deref().cnt -= 1;
        }
        if self.deref().cnt == 0 {
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
