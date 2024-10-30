//! End-to-end tests.

#![cfg(test)]
#![expect(clippy::disallowed_methods, clippy::needless_raw_string_hashes)]

mod array;
mod check;
mod comment;
mod def;
mod docs;
mod dupe;
mod function;
mod hover;
mod keyword_lit;
mod misc;
mod number;
mod object;
mod op;
mod repo;
mod smoke;
mod std_lib;
mod string;
mod unused;
mod website;
