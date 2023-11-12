//! The state of the lexer.

use crate::error::{self, Error};
use jsonnet_escape::State;

#[derive(Debug, Default)]
pub(crate) struct St<'a> {
  pub(crate) inner: str_process::St<'a>,
  errors: Vec<Error>,
}

impl<'a> St<'a> {
  pub(crate) fn new(s: &'a str) -> St<'a> {
    St { inner: str_process::St::new(s), errors: Vec::new() }
  }

  pub(crate) fn err(&mut self, kind: error::Kind) {
    self.errors.push(Error { idx: self.inner.cur_idx(), kind });
  }

  pub(crate) fn finish(self) -> Vec<Error> {
    self.errors
  }
}

impl<'a> State for St<'a> {
  fn cur(&mut self) -> Option<u8> {
    self.inner.cur()
  }

  fn bump(&mut self) {
    self.inner.bump();
  }

  fn err(&mut self, e: jsonnet_escape::Error) {
    self.err(error::Kind::Escape(e));
  }

  fn output(&mut self, _: u8) {}
}
