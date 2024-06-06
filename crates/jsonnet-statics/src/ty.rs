//! A rudimentary type system for Jsonnet.

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}
mod display;

use always::{always, convert};
use jsonnet_expr::{ExprMust, Id, Prim, Str};
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
  /// A boolean.
  Bool,
  /// A string.
  String,
  /// A number.
  Number,
  /// A specific primitive.
  ///
  /// We say that `1` has type `1` and `"foo"` has type `"foo"`, etc This is mildly strange - why
  /// don't we say `1` has type `number` and `"foo"` has type `string`, etc? Well, we could **also**
  /// say that, but this lets us be more specific when we know more.
  Prim(Prim),
  /// An array of elements, where each element has the given type.
  Array(Ty),
  /// An object with known fields.
  Object(Object),
  /// A function type, with some arguments and a return type.
  Fn(Fn),
  /// A meta type variable.
  ///
  /// TODO use
  #[allow(dead_code)]
  Meta(Meta),
  /// A union type.
  ///
  /// The empty union can never exist. This type is sometimes called "never" or "void".
  Union(Union),
}

impl Data {
  fn apply(&mut self, subst: &jsonnet_expr::Subst) {
    match self {
      Data::Prim(prim) => prim.apply(subst),
      Data::Any
      | Data::Bool
      | Data::String
      | Data::Number
      | Data::Array(_)
      | Data::Object(_)
      | Data::Fn(_)
      | Data::Meta(_)
      | Data::Union(_) => {}
    }
  }
}

pub(crate) type Object = BTreeMap<Str, Ty>;
pub(crate) type Union = BTreeSet<Ty>;

/// A meta type variable, to be solved by type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Meta(uniq::Uniq);

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
/// We'd like for types to be unique. That is, for two types a and b, if a != b, then a's data !=
/// b's data. But in the face of meta variables, this isn't true.
///
/// A simple example:
///
/// 1. create a fresh meta variable `m` and corresponding type `t`
/// 2. solve `m` to [`Ty::NUMBER`]
/// 3. observe `t` != `Ty::NUMBER`, yet `data(t) == data(Ty::NUMBER) == Data::Number`.
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
  /// TODO special logic to flatten `Or`s? and also handle single-element `Or`s by just returning
  /// the single inner element
  pub(crate) fn get(&mut self, data: Data) -> Ty {
    if let Some(&ret) = self.data_to_idx.get(&data) {
      return ret;
    }
    let ret = Ty::from_usize(self.idx_to_data.len());
    self.idx_to_data.push(data.clone());
    always!(self.data_to_idx.insert(data, ret).is_none());
    ret
  }

  /// returns the weak-head-canonical version of this type. i.e., its own data will NOT be a SOLVED
  /// Meta. but it may contain types whose data IS Meta.
  pub(crate) fn weak_head_canonical(&self, subst: &Subst, mut ty: Ty) -> Ty {
    loop {
      let Some(data) = self.idx_to_data.get(ty.to_usize()) else {
        always!(false, "no ty data for {ty:?}");
        break;
      };
      let Data::Meta(meta) = data else { break };
      let Some(&t) = subst.store.get(meta) else { break };
      ty = t;
    }
    ty
  }

  pub(crate) fn data(&self, subst: &Subst, ty: Ty) -> &Data {
    let ty = self.weak_head_canonical(subst, ty);
    if let Some(x) = self.idx_to_data.get(ty.to_usize()) {
      x
    } else {
      always!(false, "no ty data for {ty:?}");
      &Data::Any
    }
  }

  /// Applies a subst to this.
  pub fn apply(&mut self, subst: &jsonnet_expr::Subst) {
    for data in &mut self.idx_to_data {
      data.apply(subst);
    }
    self.data_to_idx = self
      .idx_to_data
      .iter()
      .enumerate()
      .map(|(idx, data)| (data.clone(), Ty::from_usize(idx)))
      .collect();
  }
}

/// A substitution constructed with type inference.
#[derive(Debug, Default)]
pub struct Subst {
  gen: uniq::UniqGen,
  store: FxHashMap<Meta, Ty>,
}

impl Subst {
  pub(crate) fn fresh(&mut self) -> Meta {
    Meta(self.gen.gen())
  }

  pub(crate) fn solve(&mut self, meta: Meta, ty: Ty) {
    always!(self.store.insert(meta, ty).is_none());
  }
}
