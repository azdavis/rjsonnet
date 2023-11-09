//! The dynamic semantics of Jsonnet.
//!
//! There are two main operations:
//!
//! - Execution: from Jsonnet expressions to Jsonnet values.
//! - Manifestation: from Jsonnet values to JSON values.
//!
//! These are both mutually recursive:
//!
//! - Jsonnet values are lazy. They can thus contain unexecuted Jsonnet expressions, which must be
//!   executed to produce values.
//! - During execution, we may need to manifest a Jsonnet value to convert it to a string.

#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]
#![allow(clippy::too_many_lines)]

pub mod error;
pub mod exec;
pub mod manifest;
pub mod val;
