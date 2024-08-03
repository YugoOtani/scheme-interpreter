use std::collections::linked_list::IterMut;
use std::collections::LinkedList;
use std::fs::File;
use std::vec;

use crate::ast::*;
use crate::insn::*;
use crate::parser::parse_many;
use crate::token::Token;
use anyhow::*;
use std::io::Read;

// Compiler構造体はランタイムの状態を持たない
// コンパイル途中にエラーが起こると状態の整合を取るのが面倒だから
// 複数のクロージャが同じupvalueを参照する場合

pub struct Compiler<'a> {
    cur: ScopeCompiler<'a>,
    par: LinkedList<ScopeCompiler<'a>>,
}
struct ScopeCompiler<'a> {
    insn: Vec<Insn>,
    local_vars: Vec<LocalVar<'a>>,
    upvalues: Vec<UpvalueInfo>,
}
struct LocalVar<'a> {
    name: &'a str,
    is_captured: bool,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UpvalueInfo {
    index: usize,
    is_local: bool,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VariableType {
    Local(usize),
    Upvalue(UpvalueInfo),
    Global,
}

impl<'a> ScopeCompiler<'a> {
    fn new() -> Self {
        Self {
            insn: vec![],
            local_vars: vec![],
            upvalues: vec![],
        }
    }
}

impl<'a> Compiler<'a> {
    pub fn new() -> Self {
        Self {
            cur: ScopeCompiler::new(),
            par: LinkedList::new(),
        }
    }
    pub fn compile(self, ast: &'a SExp) -> anyhow::Result<Vec<Insn>> {
        let mut insn = self.compile_toplevel(ast)?;
        insn.push(Insn::Exit);
        Ok(insn)
    }

    fn compile_toplevel(mut self, sexp: &'a SExp) -> anyhow::Result<Vec<Insn>> {
        assert!(self.cur.local_vars.is_empty());
        assert!(self.par.is_empty());
        match sexp {
            SExp::Id(id) => {
                self.compile_exp_id(&id)?;
                self.emit_insn(Insn::Print);
                Ok(self.get_bytecode())
            }
            SExp::SList(slist) => match &slist[..] {
                [] => {
                    self.emit_insn(Insn::Nil);
                    self.emit_insn(Insn::Print);
                    Ok(self.get_bytecode())
                }
                [SExp::Id(Id::Id("define")), arg @ ..] => match arg {
                    [name, value @ ..] => {
                        self.compile_define(name, value, false)?;
                        Ok(self.get_bytecode())
                    }
                    _ => bail!("[define] invalid format"),
                },
                [SExp::Id(Id::Id("load")), arg @ ..] => {
                    let fname = SExp::expect_id(expect_1arg(arg)?)?;
                    let content = read_to_string(fname)?;
                    let tkn = Token::from_str(&content)?;
                    let asts = parse_many(tkn)?;
                    let mut cmp = self;
                    for ast in &asts {
                        let insn = cmp.compile_toplevel(ast)?;
                        cmp = Compiler::new();
                        cmp.cur.insn = insn;
                    }
                    Ok(cmp.get_bytecode())
                }
                _ => {
                    self.compile_exp(sexp)?;
                    self.emit_insn(Insn::Print);
                    Ok(self.get_bytecode())
                }
            },
        }
    }

    fn compile_define(
        &mut self,
        name: &'a SExp,
        val: &'a [SExp],
        is_local: bool,
    ) -> anyhow::Result<()> {
        match name {
            SExp::Id(name) => {
                let val = expect_1arg(val)?;
                self.compile_exp(val)?;
                if is_local {
                    self.push_local(Id::expect_id(name)?);
                } else {
                    self.emit_insn(Insn::NewGlobal(name.to_string()));
                }
                Ok(())
            }
            SExp::SList(f_param) => {
                let (f, param) = match &f_param[..] {
                    [] => bail!("[define-lambda] function name is not specified"),
                    [f, param @ ..] => {
                        let f = SExp::expect_id(f)?;
                        (f, param)
                    }
                };
                let closure = self.compile_func(f, param, val)?;
                self.emit_insn(Insn::PushClosure(Box::new(closure)));
                if is_local {
                    self.push_local(f)
                } else {
                    self.emit_insn(Insn::NewGlobal(f.to_string()));
                }
                Ok(())
            }
        }
    }

    fn compile_func(
        &mut self,
        fname: &'a str,
        param: &'a [SExp],
        body: &'a [SExp],
    ) -> anyhow::Result<ClosureInfo> {
        self.new_frame();
        self.push_local(fname);
        for prm in param {
            self.push_local(SExp::expect_id(prm)?);
        }
        self.compile_body(body)?;
        self.emit_insn(Insn::Return);
        let cmp = self.pop_frame().unwrap();
        Ok(ClosureInfo {
            insn: cmp.insn,
            arity: param.len(),
            upvalues: vec![],
        })
    }
    fn compile_body(&mut self, lst: &'a [SExp]) -> anyhow::Result<()> {
        let n = lst.len();
        if lst.len() == 0 {
            bail!("[body] no body")
        }
        for e in &lst[0..n - 1] {
            match e {
                SExp::Id(_) => bail!("[body] expected define, found {:?}", e),
                SExp::SList(slist) => match &slist[..] {
                    [SExp::Id(Id::Id("define")), name, value @ ..] => {
                        self.compile_define(name, value, true)?;
                    }
                    _ => bail!("[define-lambda] function name is not specified"),
                },
            }
        }
        self.compile_exp(&lst[n - 1])
    }

    fn compile_exp_id(&mut self, id: &Id) -> anyhow::Result<()> {
        match id {
            Id::Id(id) => {
                let insn = match self.resolve_variable(id) {
                    VariableType::Global => Insn::GetGlobal(id.to_string()),
                    VariableType::Local(i) => Insn::GetLocal(i),
                    VariableType::Upvalue(upv) => Insn::GetUpvalue(upv.index),
                };
                self.emit_insn(insn);
                Ok(())
            }
            Id::StrLiteral(_) => todo!(),
            Id::Num(i) => {
                self.emit_insn(Insn::Int(*i));
                Ok(())
            }
            Id::Bool(b) => {
                self.emit_insn(if *b { Insn::True } else { Insn::False });
                Ok(())
            }
        }
    }
    //　計算スタックにexpの評価結果が1つ載っている状態で終了
    fn compile_exp(&mut self, s: &'a SExp) -> anyhow::Result<()> {
        match s {
            SExp::Id(id) => self.compile_exp_id(id),
            SExp::SList(sexps) => {
                match &sexps[..] {
                    [] => {
                        self.emit_insn(Insn::Nil);
                        return Ok(());
                    }
                    [SExp::Id(Id::Bool(_)), ..]
                    | [SExp::Id(Id::Num(_)), ..]
                    | [SExp::Id(Id::StrLiteral(_)), ..] => {
                        bail!("[exp] invalid format")
                    }
                    [SExp::Id(Id::Id("lambda")), var_exp @ ..] => {
                        match var_exp {
                            [] | [_] => bail!("[lambda] invalid number of argument"),
                            [var, body @ ..] => match var {
                                SExp::Id(_) => {
                                    // (lambda x (+ x 1))
                                    let closure = self.compile_func("", &var_exp[0..1], body)?;
                                    self.emit_insn(Insn::PushClosure(Box::new(closure)));
                                    Ok(())
                                }
                                SExp::SList(param) => {
                                    let closure = self.compile_func("", param, body)?;
                                    self.emit_insn(Insn::PushClosure(Box::new(closure)));
                                    Ok(())
                                }
                            },
                        }
                    }
                    [SExp::Id(Id::Id("'")), ..] | [SExp::Id(Id::Id("quote")), ..] => {
                        todo!()
                    }
                    [SExp::Id(Id::Id("cons")), arg @ ..] => {
                        let (car, cdr) = expect_2arg(arg)?;
                        self.compile_exp(car)?;
                        self.compile_exp(cdr)?;
                        self.emit_insn(Insn::Cons);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("car")), arg @ ..] => {
                        self.compile_exp(expect_1arg(arg)?)?;
                        self.emit_insn(Insn::Car);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("cdr")), arg @ ..] => {
                        self.compile_exp(expect_1arg(arg)?)?;
                        self.emit_insn(Insn::Cdr);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("set!")), arg @ ..] => {
                        let (id, exp) = expect_2arg(arg)?;
                        let id = SExp::expect_id(id)?;
                        self.compile_exp(exp)?;
                        let insn = match self.resolve_variable(id) {
                            VariableType::Local(i) => Insn::SetLocal(i),
                            VariableType::Global => Insn::SetGlobal(id.to_string()),
                            VariableType::Upvalue(upv) => Insn::SetUpvalue(upv.index),
                        };
                        self.emit_insn(insn);
                        self.emit_insn(Insn::NoneValue);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("set-car!")), arg @ ..] => {
                        let (cons, new_car) = expect_2arg(arg)?;
                        self.compile_exp(cons)?;
                        self.compile_exp(new_car)?; //new_car
                        self.emit_insn(Insn::SetCar);
                        self.emit_insn(Insn::NoneValue);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("set-cdr!")), arg @ ..] => {
                        let (cons, new_cdr) = expect_2arg(arg)?;
                        self.compile_exp(cons)?;
                        self.compile_exp(new_cdr)?; //new_car
                        self.emit_insn(Insn::SetCdr);
                        self.emit_insn(Insn::NoneValue);
                        Ok(())
                    }
                    [SExp::Id(Id::Id("+")), arg @ ..] => match arg {
                        [] => bail!("[+] expected one or more arguments"),
                        [e] => self.compile_exp(e),
                        [head, tail @ ..] => {
                            self.compile_exp(head)?;
                            for e in tail {
                                self.compile_exp(e)?;
                                self.emit_insn(Insn::Add);
                            }
                            Ok(())
                        }
                    },
                    [SExp::Id(Id::Id(id)), arg @ ..] => {
                        let insn = match self.resolve_variable(*id) {
                            VariableType::Global => Insn::GetGlobal(id.to_string()),
                            VariableType::Local(i) => Insn::GetLocal(i),
                            VariableType::Upvalue(upv) => Insn::GetUpvalue(upv.index),
                        };
                        self.emit_insn(insn);
                        for arg in arg {
                            self.compile_exp(arg)?;
                        }

                        self.emit_insn(Insn::Call(arg.len()));
                        Ok(())
                    }
                    e => bail!("exp {:?} is not implemented yet", e),
                }
            }
        }
    }
    fn emit_insn(&mut self, insn: Insn) {
        self.cur.insn.push(insn)
    }
    fn push_local(&mut self, s: &'a str) {
        self.cur.local_vars.push(LocalVar {
            name: s,
            is_captured: false,
        })
    }
    fn get_bytecode(self) -> Vec<Insn> {
        self.cur.insn
    }
    fn new_frame(&mut self) {
        let mut cur = ScopeCompiler::new();
        std::mem::swap(&mut cur, &mut self.cur);
        self.par.push_front(cur);
    }
    fn pop_frame(&mut self) -> Option<ScopeCompiler> {
        match self.par.pop_front() {
            None => None,
            Some(mut cmp) => {
                std::mem::swap(&mut cmp, &mut self.cur);
                Some(cmp)
            }
        }
    }
    fn resolve_local(cmp: &ScopeCompiler, id: &str) -> Option<usize> {
        for i in (0..cmp.local_vars.len()).rev() {
            if cmp.local_vars[i].name == id {
                return Some(i);
            }
        }
        None
    }
    fn resolve_variable(&mut self, id: &str) -> VariableType {
        if let Some(i) = Self::resolve_local(&mut self.cur, id) {
            return VariableType::Local(i);
        }
        if let Some(i) = Self::resolve_upvalue(&mut self.par.iter_mut(), id) {
            self.cur.upvalues.push(i);
            return VariableType::Upvalue(i);
        }
        VariableType::Global
    }
    fn add_upvalue(cmp: &mut ScopeCompiler, is_local: bool, index: usize) -> usize {
        if let Some(i) = cmp
            .upvalues
            .iter()
            .rposition(|e| e.index == index && e.is_local == is_local)
        {
            i
        } else {
            let ret = cmp.upvalues.len();
            cmp.upvalues.push(UpvalueInfo { index, is_local });
            ret
        }
    }
    fn resolve_upvalue(cmps: &mut IterMut<ScopeCompiler<'a>>, id: &str) -> Option<UpvalueInfo> {
        if let Some(cur) = cmps.next() {
            match Self::resolve_local(&cur, id) {
                None => match Self::resolve_upvalue(cmps, id) {
                    None => None,
                    Some(par) => {
                        let index = Self::add_upvalue(cur, par.is_local, par.index);
                        Some(UpvalueInfo {
                            index,
                            is_local: false,
                        })
                    }
                },
                Some(index) => {
                    cur.local_vars[index].is_captured = true;
                    Some(UpvalueInfo {
                        index,
                        is_local: true,
                    })
                }
            }
        } else {
            None
        }
    }
}

fn expect_1arg<T>(arg: &[T]) -> anyhow::Result<&T> {
    match arg {
        [x] => Ok(x),
        _ => bail!("expect 1 argument, found {}.", arg.len()),
    }
}
fn expect_2arg<T>(arg: &[T]) -> anyhow::Result<(&T, &T)> {
    match arg {
        [x, y] => Ok((x, y)),
        _ => bail!("expect 2 argument, found {}.", arg.len()),
    }
}
fn read_to_string(fname: &str) -> anyhow::Result<String> {
    let mut f = File::open(fname).context("file not found")?;

    let mut contents = String::new();
    f.read_to_string(&mut contents)
        .context("something went wrong reading the file")?;
    Ok(contents)
}

#[test]
fn upvalue_test() {
    let mut cmp = Compiler::new();
    // frame 1
    cmp.push_local("a1");
    cmp.push_local("b1");
    // frame 2
    cmp.new_frame();
    cmp.push_local("a2");
    cmp.push_local("b2");
    assert_eq!(unwrap_upv(cmp.resolve_variable("a1")), (true, 0));
    // frame 3
    cmp.new_frame();
    cmp.push_local("a3");
    cmp.push_local("b3");
    assert_eq!(unwrap_upv(cmp.resolve_variable("a2")), (true, 0));
    assert_eq!(unwrap_upv(cmp.resolve_variable("b1")), (false, 1));
    assert_eq!(unwrap_upv(cmp.resolve_variable("a1")), (false, 0));
    cmp.pop_frame();

    // frame 2
    assert_eq!(unwrap_upv(cmp.resolve_variable("a1")), (true, 0));
    assert_eq!(unwrap_upv(cmp.resolve_variable("b1")), (true, 1));
}
#[cfg(test)]
fn unwrap_upv(vt: VariableType) -> (bool, usize) {
    match vt {
        VariableType::Upvalue(UpvalueInfo { is_local, index }) => (is_local, index),
        _ => panic!(),
    }
}
