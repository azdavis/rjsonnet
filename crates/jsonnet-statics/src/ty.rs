//! A rudimentary type system for Jsonnet.

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}
mod display;

use always::{always, convert};
use jsonnet_expr::{ExprMust, Id, Str};
use rustc_hash::FxHashMap;
use std::collections::{BTreeMap, BTreeSet};

/// A map from expr to type.
pub type Exprs = FxHashMap<ExprMust, Ty>;

/// Data about a type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Data {
  /// Anything at all.
  ///
  /// This is like `any` in TypeScript and `T.untyped` in Sorbet (Ruby type-checker). It is BOTH a
  /// "top type" and a "bottom type". That is, anything can be coerced to it, and it can be coerced
  /// to anything. In that sense, it is the ultimate escape-hatch type.
  Any,
  /// The type of `true`.
  True,
  /// The type of `false`.
  False,
  /// The type of `null`.
  Null,
  /// A string.
  String,
  /// A number.
  Number,
  /// An array of elements, where each element has the given type.
  Array(Ty),
  /// An object, possibly with known and unknown fields.
  Object(Object),
  /// A function type, with some arguments and a return type.
  Fn(Fn),
  /// A union type.
  ///
  /// A value whose type is the empty union can never exist. This type is sometimes called "never"
  /// or "void".
  ///
  /// The union of `true` and `false` is called "bool".
  Union(Union),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Object {
  pub(crate) known: BTreeMap<Str, Ty>,
  pub(crate) has_unknown: bool,
}

impl Object {
  pub(crate) fn empty() -> Self {
    Self { known: BTreeMap::new(), has_unknown: false }
  }

  pub(crate) fn unknown() -> Self {
    Self { known: BTreeMap::new(), has_unknown: true }
  }
}

pub(crate) type Union = BTreeSet<Ty>;

/// A function type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Fn {
  /// The parameters.
  pub(crate) params: Vec<Param>,
  /// The return type.
  pub(crate) ret: Ty,
}

/// A function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Param {
  pub(crate) id: Id,
  pub(crate) ty: Ty,
  pub(crate) required: bool,
}

/// A type.
///
/// Internally represented as an index that is cheap to copy.
///
/// Types are unique. That is, for two types a and b, if a != b, then a's data != b's data.
///
/// BUT NOTE that the SEMANTICS of a type can be the same as another type but the types are
/// different, like `{ a: int } | { a: string }` and `{ a: int | string }`
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty(u32);

impl Ty {
  fn from_usize(n: usize) -> Self {
    Self(convert::usize_to_u32(n))
  }

  fn to_usize(self) -> usize {
    convert::u32_to_usize(self.0)
  }
}

/// A store of types.
///
/// Use this to make new types from data, and get the data for a type.
#[derive(Debug)]
pub struct Store {
  idx_to_data: Vec<Data>,
  data_to_idx: FxHashMap<Data, Ty>,
}

impl Store {
  pub(crate) fn get(&mut self, data: Data) -> Ty {
    match data {
      Data::Any => Ty::ANY,
      Data::True => Ty::TRUE,
      Data::False => Ty::FALSE,
      Data::Null => Ty::NULL,
      Data::String => Ty::STRING,
      Data::Number => Ty::NUMBER,
      Data::Array(_) | Data::Object(_) | Data::Fn(_) => self.get_inner(data),
      Data::Union(work) => {
        let mut work: Vec<_> = work.into_iter().collect();
        let mut parts = BTreeSet::<Ty>::new();
        while let Some(ty) = work.pop() {
          match self.data(ty) {
            Data::Any => return Ty::ANY,
            Data::Union(parts) => work.extend(parts),
            _ => {
              parts.insert(ty);
            }
          }
        }
        if parts.len() == 1 {
          match parts.pop_first() {
            None => {
              always!(false, "just checked len == 1");
              Ty::ANY
            }
            Some(ty) => ty,
          }
        } else {
          self.get_inner(Data::Union(parts))
        }
      }
    }
  }

  fn get_inner(&mut self, data: Data) -> Ty {
    match self.data_to_idx.get(&data) {
      None => {
        let ret = Ty::from_usize(self.idx_to_data.len());
        self.idx_to_data.push(data.clone());
        always!(self.data_to_idx.insert(data, ret).is_none());
        ret
      }
      Some(&ty) => ty,
    }
  }

  pub(crate) fn data(&self, ty: Ty) -> &Data {
    match self.idx_to_data.get(ty.to_usize()) {
      None => {
        always!(false, "no ty data for {ty:?}");
        &Data::Any
      }
      Some(x) => x,
    }
  }
}
