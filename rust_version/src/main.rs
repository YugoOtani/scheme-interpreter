pub mod eval;
pub mod parser;
pub mod token;
use parser::parse_token;
use std::io::{stdin, stdout, Write};

fn main() {
    for i in 1.. {
        print!("mini-scheme[{i}] > ");
        stdout().flush().unwrap();
        let mut buf = String::new();
        stdin()
            .read_line(&mut buf)
            .map_err(|e| e.to_string())
            .unwrap();
        println!("{:?}", parse_token(&buf[..]))
    }
}
