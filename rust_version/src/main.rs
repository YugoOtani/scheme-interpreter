pub mod parser;
pub mod token;
use parser::parse;
use std::{
    fmt::Display,
    io::{stdin, stdout, Write},
};
fn main() {
    /*for i in 1.. {
        print!("mini-scheme[{i}] > ");
        stdout().flush().unwrap();
        let mut buf = String::new();
        stdin()
            .read_line(&mut buf)
            .map_err(|e| e.to_string())
            .unwrap();
    }*/
}
fn test<T>(t: T) -> T
where
    T: Display,
{
    t
}
