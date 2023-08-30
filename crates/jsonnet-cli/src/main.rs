//! A small CLI.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use std::io::Read as _;

fn main() {
  let mut stdin = std::io::stdin().lock();
  let mut buf = String::new();
  stdin.read_to_string(&mut buf).expect("io err");
  let lex = jsonnet_lex::get(&buf);
  let parse = jsonnet_parse::get(&lex.tokens);
  eprintln!("{:#?}", parse.root);
  for e in &lex.errors {
    eprintln!("{e}");
  }
  for e in &parse.errors {
    eprintln!("{e:?}");
  }
  let ok = lex.errors.is_empty() && parse.errors.is_empty();
  if !ok {
    std::process::exit(1);
  }
}
