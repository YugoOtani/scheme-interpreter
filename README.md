# Scheme Interpreter written in Rust
## How to build
1. You need Rust environment. https://www.rust-lang.org/ja/tools/install
2. run `cargo run` in `rust-2023`, `rust-2024` directory.

## TODO
* Garbage Collection is not implemented properly.
* AST should be changed to S-Expression in order to implement macro effectively.
* Symbol table is implemented as a HashMap, but other data structure should be used.
