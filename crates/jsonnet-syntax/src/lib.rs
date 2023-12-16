//! Concrete syntax for Jsonnet.

#![allow(missing_debug_implementations, missing_docs)]

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod ast {
  include!(concat!(env!("OUT_DIR"), "/ast.rs"));
}

#[allow(clippy::pedantic, missing_debug_implementations, missing_docs)]
pub mod kind {
  include!(concat!(env!("OUT_DIR"), "/kind.rs"));
}
