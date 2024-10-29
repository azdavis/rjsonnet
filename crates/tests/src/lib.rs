//! End-to-end tests.

#![cfg(test)]
#![expect(clippy::disallowed_methods, clippy::needless_raw_string_hashes)]

mod check;
mod docs;
mod hover;
mod misc;
mod repo;
mod smoke;
mod unused;
mod website;
