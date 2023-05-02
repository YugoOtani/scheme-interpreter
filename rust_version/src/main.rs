pub mod dbg_token;
pub mod eval;
pub mod frame;
pub mod parser;
pub mod scheme_fn;
pub mod token;
use crate::parser::parse_tkns;
use crate::token::Toplevel;
use dbg_token::*;
use frame::{root_frame, Frame};
use parser::parse_token;
use std::cell::RefCell;
use std::fs::File;
use std::io::Read;
use std::io::{stdin, stdout, Write};
use std::rc::Rc;
fn main() {
    let frame = Rc::new(RefCell::new(root_frame()));
    for i in 1.. {
        print!("mini-scheme[{i}] > ");
        stdout().flush().unwrap();
        let mut buf = String::new();
        let input = loop {
            stdin()
                .read_line(&mut buf)
                .map_err(|e| e.to_string())
                .unwrap();

            if buf.trim().ends_with(';') {
                break buf.trim().strip_suffix(';').unwrap();
            }
        };
        match parse_token(&input) {
            Err(e) => println!("{e}"),
            Ok(Toplevel::Load(fname)) => match exec_load(fname, frame.clone()) {
                Ok(msg) => println!("{msg}"),
                Err(msg) => println!("{msg}"),
            },
            Ok(s) => match s.eval(frame.clone()) {
                Ok(res) => {
                    //s.tdbg(0);
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

fn exec_load(fname: String, frame: Rc<RefCell<Frame>>) -> Result<String, String> {
    let fname2 = fname.clone();
    let content = read_file(fname)?;
    let tkns = parse_tkns(&content)?;
    for tkn in tkns {
        tkn.eval(frame.clone())?;
    }
    Ok(format!("load [{fname2}] success"))
}

fn read_file(fname: String) -> Result<String, String> {
    let fname2 = fname.clone();
    let mut file = File::open(fname).map_err(|e| format!("error opening file [{}]", e))?;
    let mut content = String::new();
    file.read_to_string(&mut content)
        .map_err(|e| format!("error reading content of file[{fname2}] : {}", e))?;
    Ok(content)
}
