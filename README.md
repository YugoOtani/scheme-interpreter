# Scheme Interpreter

## rust-2023
* 機能: 整数、シンボル、文字列、Pair、特殊形式(lambda、let、let*、letrec、if、cond、and、or、begin、do)、基本的な関数
* 実装: 抽象構文木の走査によって式を評価・GCあり(既知のバグあり)
* TODO: 抽象構文木をS式にする・GCのバグ修正

## rust-2024
* 機能: 整数、文字列、Pair、クロージャ、特殊形式(lambda、let)
* 実装: バイトコードに変換して式を評価・GC未実装
* TODO: 基本的な機能の実装、GCの実装
  
## ビルド方法 
1. Rust実行環境のインストール https://www.rust-lang.org/ja/tools/install
2.  `rust-2023`, `rust-2024` ディレクトリで`cargo run`を実行
