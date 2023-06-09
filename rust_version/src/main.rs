pub mod dbg_token;
pub mod env;
pub mod eval;
pub mod gc;
pub mod parser;
pub mod scheme_fn;
pub mod token;
pub mod tosexp;

use crate::env::Env;
use crate::gc::mark_and_sweep;
use crate::gc::print_mem_usage;
use crate::parser::parse_tkns;
use crate::token::Toplevel;
use anyhow::anyhow;
use anyhow::Result;
use parser::parse_token;
use std::fs::File;
use std::io::Read;
use std::io::{stdin, stdout, Write};
use std::time::Instant;
const CONSOLE: &str = "mini-scheme > ";
fn main() {
    gc::init();
    let mut env = Env::new();

    for _ in 0.. {
        stdout().flush().unwrap();
        print!("{CONSOLE}");
        stdout().flush().unwrap();
        let mut input = String::new();
        loop {
            let mut buf = String::new();
            stdin()
                .read_line(&mut buf)
                .map_err(|e| e.to_string())
                .unwrap();

            if buf.trim().is_empty() {
                break;
            } else {
                input.push_str(&buf);
                if let Some(i) = indent_help(&input) {
                    for _ in 0..(i + CONSOLE.len() + 1) {
                        print!(" ");
                        stdout().flush().unwrap();
                    }
                }
            }
        }
        //pr();
        match parse_token(&input) {
            Err(e) => println!("{e}"),
            Ok(Toplevel::Load(fname)) => {
                match exec_load(fname, &mut env) {
                    Ok(msg) => {
                        println!("");
                        println!("{msg}")
                    }
                    Err(msg) => println!("{msg}"),
                };
            }
            Ok(s) => {
                //s.tdbg(0);
                let start = Instant::now();
                let res = s.eval(&mut env);
                let end = Instant::now();

                match res {
                    Ok(res) => {
                        println!("{}", res.to_string());
                        println!(
                            "execution time : {}ms",
                            end.duration_since(start).as_millis()
                        );
                    }
                    Err(msg) => {
                        println!("{}", msg);
                    }
                }
            }
        }
        mark_and_sweep();
        print_mem_usage();
    }
}

fn exec_load(fname: String, env: &mut Env) -> Result<String> {
    let fname2 = fname.clone();
    let content = read_file(fname)?;
    let tkns = parse_tkns(&content)?;
    for tkn in tkns {
        tkn.eval(env)?;
    }
    Ok(format!("load [{fname2}] success"))
}

fn read_file(fname: String) -> Result<String> {
    let fname2 = fname.clone();
    let mut file = File::open(fname).map_err(|e| anyhow!("{} : {}", fname2.to_string(), e))?;
    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err(|e| anyhow!("{} : {}", fname2.to_string(), e))?;
    Ok(content)
}

fn indent_help(s: &str) -> Option<usize> {
    let mut check = vec![false; s.len()];
    let mut isin_string = false;
    for i in 0..s.len() {
        match s.chars().nth(i).unwrap() {
            '"' => {
                check[i] = true;
                isin_string = !isin_string
            }
            '(' => {}
            ')' => {
                check[i] = true;
                if !isin_string {
                    match check[0..i].iter().rposition(|c| !*c) {
                        Some(x) => check[x] = true,
                        None => return None,
                    }
                }
            }
            _ => check[i] = true,
        }
    }
    match check.iter().rposition(|c| !*c) {
        Some(i) => match (&s[..i]).rfind(|c| c == '\n') {
            None => Some(i),
            Some(j) => Some(i - j),
        },
        None => Some(0),
    }
}
