//! End-to-end tests.

#![cfg(test)]
#![allow(clippy::disallowed_methods, clippy::needless_raw_string_hashes)]
// TODO turn this off once most tests are OK
#![allow(clippy::should_panic_without_expect)]

mod check;
mod misc;
mod repo;
mod smoke;
mod website;
