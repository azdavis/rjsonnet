//! Execution of jsonnet expression to produce Jsonnet values, and manifestation of those values
//! into JSON values.
//!
//! From the [spec](https://jsonnet.org/ref/spec.html).

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

pub mod exec;
pub mod manifest;
pub mod val;

use jsonnet_expr::Str;

#[derive(Debug)]
pub enum Error {
  Todo,
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument,
  DuplicateField,
  IncompatibleTypes,
  NoSuchArgument,
  NoSuchFieldName,
  TooManyArguments,
  CmpNan,
  CmpInf,
  User(Str),
  NoExpr,
  Function,
}

pub type Eval<T> = Result<T, Error>;
