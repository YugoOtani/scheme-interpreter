pub mod dbg_token;
pub mod env;
pub mod eval;
pub mod parser;
pub mod scheme_fn;
pub mod token;
use dbg_token::*;
use env::root_env;
use parser::parse_token;
use std::cell::RefCell;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;
fn main() {
    let env = Rc::new(RefCell::new(root_env()));
    for i in 1.. {
        print!("mini-scheme[{i}] > ");
        stdout().flush().unwrap();
        let mut buf = String::new();
        stdin()
            .read_line(&mut buf)
            .map_err(|e| e.to_string())
            .unwrap();
        match parse_token(&buf[..]) {
            Err(e) => println!("{e}"),
            Ok(s) => match s.eval(env.clone()) {
                Ok(res) => {
                    println!("{}", res.as_ref().to_string());
                }
                Err(msg) => {
                    println!("{}", msg);
                    s.tdbg(0);
                }
            },
        }
    }
}
