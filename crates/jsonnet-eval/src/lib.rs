//! The dynamic semantics of Jsonnet.
//!
//! From the [spec](https://jsonnet.org/ref/spec.html).

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

pub mod exec;
pub mod manifest;
pub mod val;
