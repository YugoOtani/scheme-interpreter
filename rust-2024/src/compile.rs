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

pub struct Compiler<'a> {
    cur: ScopeCompiler<'a>,
    par: LinkedList<ScopeCompiler<'a>>,
}
struct ScopeCompiler<'a> {
    insn: Vec<Insn>,
    local_vars: Vec<&'a str>,
}
#[derive(Debug, Clone, Copy)]
enum VariableType {
    Local(usize),
    Upvalue { from_cur: usize, index: usize },
    Global,
}
impl<'a> ScopeCompiler<'a> {
    fn new() -> Self {
        Self {
            insn: vec![],
            local_vars: vec![],
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
                [SExp::Id(Id::Id("define")), ..] => {
                    self.compile_define(slist, false)?;
                    Ok(self.get_bytecode())
                }
                [SExp::Id(Id::Id("load")), ..] => {
                    let fname = Self::expect_1_arg(&slist[..])?;
                    let fname = Self::expect_id(fname)?;

                    let content = Self::read_to_string(fname)?;
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

    fn compile_define(&mut self, lst: &'a Vec<SExp>, is_local: bool) -> anyhow::Result<()> {
        assert!(lst.len() != 0 && matches!(&lst[0], SExp::Id(Id::Id("define"))));
        match &lst[..] {
            [] | [_] | [_, _] => bail!("[define] invalid numer of argument"),
            [_, SExp::Id(Id::Id(id)), exp] => {
                self.compile_exp(exp)?;
                self.emit_insn(Insn::NewGlobal(id.to_string()));

                Ok(())
            }
            [_, f_arg, body @ ..] => {
                match f_arg {
                    SExp::SList(f_arg) => {
                        // (define (??) body)
                        match &f_arg[..] {
                            [] => bail!("[define-lambda] function name is not specified"),
                            [f, arg @ ..] => {
                                let f = Self::expect_id(f)?;
                                let insn = self.compile_func(f, arg, body)?;
                                self.emit_insn(Insn::MkClosure(insn));
                                if !is_local {
                                    self.emit_insn(Insn::NewGlobal(f.to_string()));
                                } else {
                                    self.push_local(f)
                                }
                                Ok(())
                            }
                        }
                    }
                    SExp::Id(_) => bail!("[define-lambda] invalid format"),
                }
            }
        }
    }

    fn compile_func(
        &mut self,
        fname: &'a str,
        arg: &'a [SExp],
        body: &'a [SExp],
    ) -> anyhow::Result<Vec<Insn>> {
        self.new_frame();
        self.push_local(fname);
        for arg in arg {
            self.push_local(Self::expect_id(arg)?);
        }
        self.compile_body(body)?;
        self.emit_insn(Insn::Return);
        let cmp = self.pop_frame().unwrap();
        Ok(cmp.insn)
    }
    fn compile_body(&mut self, lst: &'a [SExp]) -> anyhow::Result<()> {
        let n = lst.len();
        if lst.len() == 0 {
            bail!("[body] no body")
        } else {
            for e in &lst[0..n - 1] {
                match e {
                    SExp::Id(_) => bail!("[body] expected define, found {:?}", e),
                    SExp::SList(slist) => self.compile_define(slist, true)?,
                }
            }
            self.compile_exp(&lst[n - 1])
        }
    }

    fn compile_exp_id(&mut self, id: &Id) -> anyhow::Result<()> {
        match id {
            Id::Id(id) => {
                let insn = match self.resolve_variable(id) {
                    VariableType::Global => Insn::GetGlobal(id.to_string()),
                    VariableType::Local(i) => Insn::GetLocal(i),
                    VariableType::Upvalue { .. } => todo!(),
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
    fn compile_exp(&mut self, s: &'a SExp) -> anyhow::Result<()> {
        match s {
            SExp::Id(id) => self.compile_exp_id(id),
            SExp::SList(sexps) => {
                if sexps.len() == 0 {
                    self.emit_insn(Insn::Nil);
                    return Ok(());
                }
                match &sexps[0] {
                    SExp::Id(Id::Bool(_) | Id::Num(_) | Id::StrLiteral(_)) => {
                        bail!("[exp] invalid format")
                    }
                    SExp::Id(Id::Id("lambda")) => match &sexps[1..] {
                        [] | [_] => bail!("[lambda] invalid number of argument"),
                        [arg, ..] => match arg {
                            SExp::Id(_) => {
                                let fname = Self::expect_id(arg)?;
                                let insn = self.compile_func(fname, &[], &sexps[2..])?;
                                self.emit_insn(Insn::MkClosure(insn));
                                Ok(())
                            }
                            SExp::SList(_) => todo!(),
                        },
                    },
                    SExp::Id(Id::Id("'")) | SExp::Id(Id::Id("quote")) => {
                        todo!()
                    }
                    SExp::Id(Id::Id("cons")) => {
                        let (car, cdr) = Self::expect_2_arg(&sexps[..])?;
                        self.compile_exp(car)?;
                        self.compile_exp(cdr)?;
                        self.emit_insn(Insn::Cons);
                        Ok(())
                    }
                    SExp::Id(Id::Id("car")) => {
                        let lst = Self::expect_1_arg(&sexps[..])?;
                        self.compile_exp(lst)?;
                        self.emit_insn(Insn::Car);
                        Ok(())
                    }
                    SExp::Id(Id::Id("cdr")) => {
                        let lst = Self::expect_1_arg(&sexps[..])?;
                        self.compile_exp(lst)?;
                        self.emit_insn(Insn::Cdr);
                        Ok(())
                    }
                    SExp::Id(Id::Id("set!")) => {
                        let (id, exp) = Self::expect_2_arg(&sexps[..])?;
                        let id = Self::expect_id(id)?;
                        self.compile_exp(exp)?;
                        let insn = match Self::resolve_variable(&self, id) {
                            VariableType::Local(i) => Insn::SetLocal(i),
                            VariableType::Global => Insn::SetGlobal(id.to_string()),
                            VariableType::Upvalue { from_cur, index } => {
                                Insn::SetUpValue(self.to_upvalue_index(from_cur, index))
                            }
                        };
                        self.emit_insn(insn);
                        Ok(())
                    }
                    SExp::Id(Id::Id("+")) => match &sexps[1..] {
                        [] => bail!("[+] expected one or more arguments"),
                        [e] => self.compile_exp(e),
                        [fst, ..] => {
                            self.compile_exp(fst)?;
                            for e in &sexps[2..] {
                                self.compile_exp(e)?;
                                self.emit_insn(Insn::Add);
                            }
                            Ok(())
                        }
                    },
                    SExp::Id(Id::Id(id)) => {
                        let insn = match self.resolve_variable(*id) {
                            VariableType::Global => Insn::GetGlobal(id.to_string()),
                            VariableType::Local(i) => Insn::GetLocal(i),
                            VariableType::Upvalue { .. } => todo!(), //関数呼び出しかつクロージャ
                        };
                        self.emit_insn(insn);
                        for i in 1..sexps.len() {
                            self.compile_exp(&sexps[i])?;
                        }

                        self.emit_insn(Insn::Call(sexps.len() - 1));
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
        self.cur.local_vars.push(s)
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
    fn expect_id<'c>(sexp: &'c SExp) -> anyhow::Result<&'c str> {
        match sexp {
            SExp::Id(Id::Id(id)) => Ok(id),
            _ => bail!("expect id, found {:?}", sexp),
        }
    }

    fn expect_1_arg<'c, 'd>(arg: &'c [SExp<'d>]) -> anyhow::Result<&'c SExp<'c>> {
        match arg {
            [_, x] => Ok(x),
            _ => bail!("expect 1 argument, found {}.", arg.len()),
        }
    }
    fn expect_2_arg<'c, 'd>(arg: &'c [SExp<'d>]) -> anyhow::Result<(&'c SExp<'d>, &'c SExp<'d>)> {
        match arg {
            [_, x, y] => Ok((x, y)),
            _ => bail!("expect 1 argument, found {}.", arg.len()),
        }
    }
    fn read_to_string(fname: &str) -> anyhow::Result<String> {
        let mut f = File::open(fname).context("file not found")?;

        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .context("something went wrong reading the file")?;
        Ok(contents)
    }
    fn resolve_variable(&self, id: &str) -> VariableType {
        for i in (0..self.cur.local_vars.len()).rev() {
            if self.cur.local_vars[i] == id {
                return VariableType::Local(i);
            }
        }
        for (cmp_i, cmp) in self.par.iter().enumerate() {
            for local_i in (0..cmp.local_vars.len()).rev() {
                if cmp.local_vars[local_i] == id {
                    return VariableType::Upvalue {
                        from_cur: cmp_i + 1,
                        index: local_i,
                    };
                }
            }
        }
        VariableType::Global
    }
    fn to_upvalue_index(&self, _frame_index: usize, _var_index: usize) -> usize {
        todo!()
    }
}
