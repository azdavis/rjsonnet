//! Concrete syntax for Jsonnet.

#![deny(rust_2018_idioms)]

pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}
pub mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}
