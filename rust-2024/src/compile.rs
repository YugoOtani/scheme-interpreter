use std::fs::File;

use crate::ast::*;
use crate::insn::*;
use crate::parser::parse_many;
use crate::token::Token;
use anyhow::*;
use std::io::Read;

// Compiler構造体はランタイムの状態を持たない
// コンパイル途中にエラーが起こると状態の整合を取るのが面倒だから

pub struct Compiler {
    insn: Vec<Insn>,
    local_vars: Vec<String>,
    parent: Option<Box<Compiler>>,
}
#[derive(Debug, Clone, Copy)]
enum VariableType {
    Local(usize),
    Upvalue { from_cur: usize, index: usize },
    Global,
}
impl Compiler {
    pub fn new() -> Self {
        Self {
            insn: vec![],
            local_vars: vec![],
            parent: None,
        }
    }
    pub fn compile(&mut self, ast: &SExp) -> anyhow::Result<Vec<Insn>> {
        let mut ret = vec![];
        self.compile_toplevel(ast)?;
        std::mem::swap(&mut ret, &mut self.insn);
        Ok(ret)
    }
    fn compile_toplevel(&mut self, sexp: &SExp) -> anyhow::Result<()> {
        assert!(self.insn.is_empty());
        match sexp {
            SExp::Id(id) => {
                self.compile_exp_id(&id)?;
                self.insn.push(Insn::Exit);
                Ok(())
            }
            SExp::SList(slist) => match &slist[..] {
                [] => {
                    self.insn.push(Insn::Nil);
                    Ok(())
                }
                [SExp::Id(Id::Id("define")), ..] => {
                    self.compile_define(slist, false)?;
                    self.insn.push(Insn::NoneValue);
                    self.insn.push(Insn::Exit);
                    Ok(())
                }
                [SExp::Id(Id::Id("load")), ..] => {
                    self.compile_load(slist)?;
                    self.insn.push(Insn::NoneValue);
                    self.insn.push(Insn::Exit);
                    Ok(())
                }
                _ => {
                    self.compile_exp(sexp)?;
                    self.insn.push(Insn::Exit);
                    Ok(())
                }
            },
        }
    }

    fn compile_define(&mut self, lst: &Vec<SExp>, is_local: bool) -> anyhow::Result<()> {
        assert!(lst.len() != 0 && matches!(&lst[0], SExp::Id(Id::Id("define"))));
        match &lst[..] {
            [] | [_] | [_, _] => bail!("[define] invalid numer of argument"),
            [_, SExp::Id(Id::Id(id)), exp] => {
                self.compile_exp(exp)?;
                self.insn.push(Insn::NewGlobal(id.to_string()));

                Ok(())
            }
            _ => {
                let id = &lst[1];
                match id {
                    SExp::SList(slist) => {
                        let n = slist.len();

                        if n == 0 {
                            bail!("[define-lambda] function name is not specified")
                        }
                        let fname = Self::expect_id(&slist[0])?;
                        for arg in &slist[1..] {
                            let argname = Self::expect_id(arg)?;
                            self.local_vars.push(argname.to_string());
                        }
                        let body = self.compile_func_body(&lst[2..])?;
                        for _ in 0..n - 1 {
                            self.local_vars.pop().unwrap();
                        }
                        self.insn.push(Insn::MkClosure(body));
                        if is_local {
                            self.local_vars.push(fname.to_string());
                            Ok(())
                        } else {
                            self.insn.push(Insn::NewGlobal(fname.to_string()));
                            Ok(())
                        }
                    }
                    SExp::Id(_) => bail!("[define-lambda] invalid format"),
                }
            }
        }
    }
    // TODO : resolve upvalue
    fn compile_func_body(&mut self, lst: &[SExp]) -> anyhow::Result<Vec<Insn>> {
        let mut tmp = vec![];
        std::mem::swap(&mut tmp, &mut self.insn);
        self.compile_body(lst)?;
        std::mem::swap(&mut tmp, &mut self.insn);
        Ok(tmp)
    }
    fn compile_func(&mut self, fname: &str, arg: &[SExp]) {
        todo!()
    }
    fn compile_body(&mut self, lst: &[SExp]) -> anyhow::Result<()> {
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
    fn compile_load(&mut self, lst: &Vec<SExp>) -> anyhow::Result<()> {
        assert!(lst.len() != 0 && matches!(&lst[0], SExp::Id(Id::Id("load"))));
        let fname = Self::expect_1_arg(&lst[..])?;
        let fname = match fname {
            SExp::Id(Id::Id(s)) => s,
            _ => bail!("[load] expected file name, found {:?}", fname),
        };
        println!("loading {}", fname);
        let content = Self::read_to_string(fname)?;
        let tkn = Token::from_str(&content)?;
        let asts = parse_many(tkn)?;
        for ast in &asts {
            self.compile_toplevel(ast)?;
        }
        Ok(())
    }

    fn compile_exp_id(&mut self, id: &Id) -> anyhow::Result<()> {
        match id {
            Id::Id(id) => {
                let insn = match self.resolve_variable(id) {
                    VariableType::Global => Insn::GetGlobal(id.to_string()),
                    VariableType::Local(i) => Insn::GetLocal(i),
                    VariableType::Upvalue { .. } => todo!(),
                };
                self.insn.push(insn);
                Ok(())
            }
            Id::StrLiteral(_) => todo!(),
            Id::Num(i) => {
                self.insn.push(Insn::Int(*i));
                Ok(())
            }
            Id::Bool(b) => {
                self.insn.push(if *b { Insn::True } else { Insn::False });
                Ok(())
            }
        }
    }
    fn compile_exp(&mut self, s: &SExp) -> anyhow::Result<()> {
        match s {
            SExp::Id(id) => self.compile_exp_id(id),
            SExp::SList(sexps) => {
                if sexps.len() == 0 {
                    self.insn.push(Insn::Nil);
                    return Ok(());
                }
                match &sexps[0] {
                    SExp::Id(Id::Bool(_) | Id::Num(_) | Id::StrLiteral(_)) => {
                        bail!("[exp] invalid format")
                    }
                    SExp::Id(Id::Id("lambda")) => {
                        todo!()
                    }
                    SExp::Id(Id::Id("'")) | SExp::Id(Id::Id("quote")) => {
                        todo!()
                    }
                    SExp::Id(Id::Id("cons")) => {
                        let (car, cdr) = Self::expect_2_arg(&sexps[..])?;
                        self.compile_exp(car)?;
                        self.compile_exp(cdr)?;
                        self.insn.push(Insn::Cons);
                        Ok(())
                    }
                    SExp::Id(Id::Id("car")) => {
                        let lst = Self::expect_1_arg(&sexps[..])?;
                        self.compile_exp(lst)?;
                        self.insn.push(Insn::Car);
                        Ok(())
                    }
                    SExp::Id(Id::Id("cdr")) => {
                        let lst = Self::expect_1_arg(&sexps[..])?;
                        self.compile_exp(lst)?;
                        self.insn.push(Insn::Cdr);
                        Ok(())
                    }
                    SExp::Id(Id::Id("set!")) => {
                        let (id, exp) = Self::expect_2_arg(&sexps[..])?;
                        let id = Self::expect_id(id)?;
                        self.compile_exp(exp)?;
                        let insn = match Self::resolve_variable(&self, id) {
                            VariableType::Local(i) => Insn::SetLocal(i),
                            VariableType::Global => Insn::SetGlobal(id.to_string()),
                            VariableType::Upvalue { .. } => todo!(),
                        };
                        self.insn.push(insn);
                        Ok(())
                    }
                    SExp::Id(Id::Id("+")) => match &sexps[1..] {
                        [] => bail!("[+] expected one or more arguments"),
                        [e] => self.compile_exp(e),
                        [fst, ..] => {
                            self.compile_exp(fst)?;
                            for e in &sexps[2..] {
                                self.compile_exp(e)?;
                                self.insn.push(Insn::Add);
                            }
                            Ok(())
                        }
                    },
                    SExp::Id(Id::Id(id)) => {
                        for i in 1..sexps.len() {
                            self.compile_exp(&sexps[i])?;
                        }
                        let insn = match self.resolve_variable(*id) {
                            VariableType::Global => Insn::GetGlobal(id.to_string()),
                            VariableType::Local(i) => Insn::GetLocal(i),
                            VariableType::Upvalue { .. } => todo!(),
                        };
                        self.insn.push(insn);
                        self.insn.push(Insn::Call(sexps.len() - 1));
                        Ok(())
                    }
                    e => bail!("exp {:?} is not implemented yet", e),
                }
            }
        }
    }
    fn expect_id<'b>(sexp: &'b SExp) -> anyhow::Result<&'b str> {
        match sexp {
            SExp::Id(Id::Id(id)) => Ok(id),
            _ => bail!("expect id, found {:?}", sexp),
        }
    }

    fn expect_1_arg<'b, 'c>(arg: &'b [SExp<'c>]) -> anyhow::Result<&'b SExp<'c>> {
        match arg {
            [_, x] => Ok(x),
            _ => bail!("expect 1 argument, found {}.", arg.len()),
        }
    }
    fn expect_2_arg<'b, 'c>(arg: &'b [SExp<'c>]) -> anyhow::Result<(&'b SExp<'c>, &'b SExp<'c>)> {
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
        for i in (0..self.local_vars.len()).rev() {
            if self.local_vars[i] == id {
                return VariableType::Local(i);
            }
        }
        let mut f = &self.parent;
        let mut cnt = 1;
        while let Some(frame) = f {
            for i in (0..frame.local_vars.len()).rev() {
                if frame.local_vars[i] == id {
                    return VariableType::Upvalue {
                        from_cur: cnt,
                        index: i,
                    };
                }
            }
            cnt += 1;
            f = &frame.parent;
        }
        VariableType::Global
    }
}
