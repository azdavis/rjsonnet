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
  pub(crate) fn get(&mut self, subst: &Subst, data: Data) -> Ty {
    let ret = Ty::from_usize(self.idx_to_data.len());
    self.idx_to_data.push(data);
    // see if we can simplify the data before inserting it into `data_to_idx`.
    let data = self.data(subst, ret);
    // now THIS is wild. after simplifying the data, we may find there's already a type for it.
    match self.data_to_idx.get(&data) {
      None => {
        always!(self.data_to_idx.insert(data, ret).is_none());
        ret
      }
      Some(&ty) => {
        always!(ty != ret);
        always!(self.idx_to_data.pop().is_some());
        ty
      }
    }
  }

  pub(crate) fn data(&self, subst: &Subst, ty: Ty) -> Data {
    let mut bool = BoolUnion::None;
    let mut string = PrimUnion::Set(BTreeSet::new());
    let mut number = PrimUnion::Set(BTreeSet::new());
    let mut parts = BTreeSet::<Ty>::new();
    let mut work = vec![ty];
    while let Some(ty) = work.pop() {
      match self.data_unchecked(ty) {
        Data::Any => return Data::Any,
        Data::Bool => bool = BoolUnion::Any,
        Data::String => string = PrimUnion::Any,
        Data::Number => number = PrimUnion::Any,
        Data::Prim(Prim::Bool(b)) => bool = bool.add(b),
        Data::Prim(Prim::String(_)) => string.add(ty),
        Data::Prim(Prim::Number(_)) => number.add(ty),
        Data::Prim(Prim::Null) | Data::Array(_) | Data::Object(_) | Data::Fn(_) => {
          // DO NOT recur.
          parts.insert(ty);
        }
        Data::Union(parts) => work.extend(parts),
        Data::Meta(meta) => match subst.store.get(&meta) {
          None => ignore(parts.insert(ty)),
          // keep dereferencing.
          Some(t) => work.push(*t),
        },
      }
    }
    match bool {
      BoolUnion::None => {}
      BoolUnion::Any => ignore(parts.insert(Ty::BOOL)),
      BoolUnion::True => ignore(parts.insert(Ty::TRUE)),
      BoolUnion::False => ignore(parts.insert(Ty::FALSE)),
    }
    match string {
      PrimUnion::Any => ignore(parts.insert(Ty::STRING)),
      PrimUnion::Set(strings) => parts.extend(strings),
    }
    match number {
      PrimUnion::Any => ignore(parts.insert(Ty::NUMBER)),
      PrimUnion::Set(numbers) => parts.extend(numbers),
    }
    if parts.len() == 1 {
      match parts.into_iter().next() {
        None => {
          always!(false, "just checked len == 1");
          Data::Any
        }
        Some(ty) => self.data_unchecked(ty),
      }
    } else {
      Data::Union(parts)
    }
  }

  fn data_unchecked(&self, ty: Ty) -> Data {
    match self.idx_to_data.get(ty.to_usize()) {
      None => {
        always!(false, "no ty data for {ty:?}");
        Data::Any
      }
      Some(x) => x.clone(),
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

enum BoolUnion {
  None,
  Any,
  True,
  False,
}

impl BoolUnion {
  fn add(self, b: bool) -> Self {
    match (self, b) {
      (Self::Any, _) | (Self::False, true) | (Self::True, false) => Self::Any,
      (Self::None | Self::True, true) => Self::True,
      (Self::None | Self::False, false) => Self::False,
    }
  }
}

enum PrimUnion {
  Any,
  Set(BTreeSet<Ty>),
}

impl PrimUnion {
  fn add(&mut self, ty: Ty) {
    match self {
      PrimUnion::Any => {}
      PrimUnion::Set(set) => ignore(set.insert(ty)),
    }
  }
}

/// so we don't have to waste space with extra `{}` and newlines and `;` for a simple match arm.
fn ignore(_: bool) {}
