//! A rudimentary type system for Jsonnet.

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use always::{always, convert};
use jsonnet_expr::{Prim, Str};
use rustc_hash::FxHashMap;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum Data {
  Any,
  Bool,
  String,
  Number,
  Prim(Prim),
  Array(Ty),
  Object {
    known: BTreeMap<Str, Ty>,
    other: bool,
  },
  Fn(Vec<Ty>, Ty),
  // TODO use
  #[allow(dead_code)]
  Or(BTreeSet<Ty>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Ty(u32);

#[derive(Debug)]
pub(crate) struct Store {
  idx_to_data: Vec<Data>,
  data_to_idx: FxHashMap<Data, Ty>,
}

impl Store {
  pub(crate) fn get(&mut self, data: Data) -> Ty {
    if let Some(&ret) = self.data_to_idx.get(&data) {
      return ret;
    }
    let ret = Ty(convert::usize_to_u32(self.idx_to_data.len()));
    self.idx_to_data.push(data.clone());
    always!(self.data_to_idx.insert(data, ret).is_none());
    ret
  }

  // TODO use
  #[allow(dead_code)]
  pub(crate) fn data(&mut self, ty: Ty) -> &Data {
    match self.idx_to_data.get(convert::u32_to_usize(ty.0)) {
      None => {
        always!(false, "no ty data for {ty:?}");
        &Data::Any
      }
      Some(x) => x,
    }
  }
}
