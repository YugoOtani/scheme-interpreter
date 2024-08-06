pub mod ast;
pub mod compile;
pub mod datastructure;
pub mod exec;
pub mod insn;
pub mod memory;
pub mod parser;
pub mod scheme_value;
pub mod token;
pub mod upvalue;
use anyhow::*;
use compile::Compiler;
use exec::*;
use parser::*;
use std::io::{self, Write};
use token::Token;
fn main() -> anyhow::Result<()> {
    let mut repl = Interpreter::new();
    loop {
        print!(" scheme > ");
        io::stdout().flush().unwrap();
        let s = get_line()?;
        if s.trim() == "" {
            continue;
        }
        match repl.interpret(&s) {
            std::result::Result::Ok(()) => println!("[OK]"),
            Err(msg) => println!("{}", msg),
        }
    }
}
struct Interpreter<'a> {
    vm: VM<'a>,
}
impl<'a> Interpreter<'a> {
    fn new() -> Self {
        Self { vm: VM::new() }
    }
    fn interpret(&mut self, input: &str) -> anyhow::Result<()> {
        println!("----------------------------");
        println!("input: {}", input.trim());
        let tkn = Token::from_str(&input)?;
        println!("----------------------------");
        println!("{:?}", tkn);
        println!("----------------------------");
        let sexp = parse(tkn)?;
        println!("{:?}", sexp);
        let cmp = Compiler::new();
        let insn = cmp.compile(&sexp)?;
        println!("----------------------------");
        println!("{:?}", insn);
        let res = self.vm.exec(insn)?;
        println!("----------result------------");
        Ok(res)
    }
}

fn get_line() -> anyhow::Result<String> {
    let mut buffer = String::new();
    io::stdin()
        .read_line(&mut buffer)
        .context("failed to get input")?;
    Ok(buffer)
}

#[cfg(debug_assertions)]
fn short_addr<T>(addr: &T) -> String {
    format!("{:x}", (addr as *const T as usize) % (1 << 16))
}
