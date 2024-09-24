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
  /// A function from the standard library. These are treated specially.
  StdFn(jsonnet_expr::StdFn),
  /// A union type.
  ///
  /// A value whose type is the empty union can never exist. This type is sometimes called "never"
  /// or "void".
  ///
  /// The union of `true` and `false` is called "bool".
  Union(Union),
}

impl Data {
  fn apply(&mut self, subst: &Subst) {
    match self {
      Data::Array(ty) => ty.apply(subst),
      Data::Object(object) => object.apply(subst),
      Data::Fn(f) => f.apply(subst),
      Data::Union(parts) => {
        *parts = parts
          .iter()
          .map(|ty| {
            let mut ret = *ty;
            ret.apply(subst);
            ret
          })
          .collect();
      }
      Data::Any
      | Data::True
      | Data::False
      | Data::Null
      | Data::String
      | Data::Number
      | Data::StdFn(_) => {}
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Object {
  pub(crate) known: BTreeMap<Str, Ty>,
  pub(crate) has_unknown: bool,
}

impl Object {
  fn apply(&mut self, subst: &Subst) {
    for ty in self.known.values_mut() {
      ty.apply(subst);
    }
  }

  pub(crate) fn empty() -> Self {
    Self { known: BTreeMap::new(), has_unknown: false }
  }

  pub(crate) fn unknown() -> Self {
    Self { known: BTreeMap::new(), has_unknown: true }
  }
}

/// A function type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Fn {
  /// The parameters.
  pub(crate) params: Vec<Param>,
  /// The return type.
  pub(crate) ret: Ty,
}

impl Fn {
  fn apply(&mut self, subst: &Subst) {
    for param in &mut self.params {
      param.apply(subst);
    }
    self.ret.apply(subst);
  }
}

/// A function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Param {
  pub(crate) id: Id,
  pub(crate) ty: Ty,
  pub(crate) required: bool,
}

impl Param {
  fn apply(&mut self, subst: &Subst) {
    self.ty.apply(subst);
  }
}

pub(crate) type Union = BTreeSet<Ty>;

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
  const LOCAL_MASK: u32 = 1u32 << 31u32;

  fn from_idx(idx: usize) -> Self {
    let ret = Self(convert::usize_to_u32(idx));
    always!(!ret.is_local(), "types should always start as not local (maybe overflow occurred?)");
    ret
  }

  fn is_local(self) -> bool {
    self.0 & Self::LOCAL_MASK == Self::LOCAL_MASK
  }

  fn to_data(mut self) -> (usize, bool) {
    // NOTE: the build script depends on the non-mut case being 0
    let is_local = self.is_local();
    // turn off the mask if it was on
    self.0 &= !Self::LOCAL_MASK;
    let idx = convert::u32_to_usize(self.0);
    (idx, is_local)
  }

  fn make_local(&mut self) {
    self.0 |= Self::LOCAL_MASK;
  }

  /// Applies a subst to this.
  pub fn apply(&mut self, subst: &Subst) {
    if let Some(new) = subst.old_to_new.get(self) {
      *self = *new;
    }
  }
}

/// A store of types.
///
/// Use this to make new types from data, and get the data for a type.
#[derive(Debug)]
struct Store {
  idx_to_data: Vec<Data>,
  data_to_idx: FxHashMap<Data, Ty>,
}

impl Store {
  fn empty() -> Self {
    Self { idx_to_data: Vec::new(), data_to_idx: FxHashMap::default() }
  }

  fn data(&self, ty: Ty, self_local: bool) -> Option<&Data> {
    let (idx, ty_local) = ty.to_data();
    if self_local != ty_local {
      return None;
    }
    match self.idx_to_data.get(idx) {
      None => {
        always!(false, "no ty data for {ty:?}");
        Some(&Data::Any)
      }
      x => x,
    }
  }
}

/// A store that allows mutation.
#[derive(Debug)]
pub(crate) struct MutStore<'a> {
  global: &'a GlobalStore,
  local: LocalStore,
}

impl<'a> MutStore<'a> {
  pub(crate) fn new(global: &'a GlobalStore) -> Self {
    Self { global, local: LocalStore::default() }
  }

  pub(crate) fn get(&mut self, data: Data) -> Ty {
    match data {
      Data::Any => Ty::ANY,
      Data::True => Ty::TRUE,
      Data::False => Ty::FALSE,
      Data::Null => Ty::NULL,
      Data::String => Ty::STRING,
      Data::Number => Ty::NUMBER,
      Data::StdFn(f) => Ty::std_fn(f),
      Data::Array(_) | Data::Object(_) | Data::Fn(_) => self.get_inner(data),
      Data::Union(work) => {
        let mut work: Vec<_> = work.into_iter().collect();
        let mut parts = BTreeSet::<Ty>::new();
        while let Some(ty) = work.pop() {
          match self.global.0.data(ty, false) {
            Some(Data::Any) => return Ty::ANY,
            Some(Data::Union(parts)) => work.extend(parts),
            None | Some(_) => {
              // don't need special handling for the None case
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
    if let Some(&ty) = self.global.0.data_to_idx.get(&data) {
      always!(!ty.is_local());
      return ty;
    }
    if let Some(&ty) = self.local.0.data_to_idx.get(&data) {
      let mut ty = ty;
      always!(!ty.is_local());
      ty.make_local();
      return ty;
    }
    let mut ret = Ty::from_idx(self.local.0.idx_to_data.len());
    self.local.0.idx_to_data.push(data.clone());
    always!(self.local.0.data_to_idx.insert(data, ret).is_none());
    ret.make_local();
    ret
  }

  pub(crate) fn data(&self, ty: Ty) -> &Data {
    let (idx, is_local) = ty.to_data();
    let store = if is_local { &self.local.0 } else { &self.global.0 };
    match store.idx_to_data.get(idx) {
      None => {
        always!(false, "should be able to get data");
        &Data::Any
      }
      Some(x) => x,
    }
  }

  pub(crate) fn into_local(self) -> LocalStore {
    self.local
  }
}

/// The global store of types.
#[derive(Debug)]
pub struct GlobalStore(Store);

impl Default for GlobalStore {
  fn default() -> Self {
    Self(Store::with_builtin())
  }
}

/// A local store of types generated in one pass.
#[derive(Debug)]
pub struct LocalStore(Store);

impl Default for LocalStore {
  fn default() -> Self {
    Self(Store::empty())
  }
}

/// A substitution between two [`Store`]s.
#[derive(Debug)]
pub struct Subst {
  old_to_new: FxHashMap<Ty, Ty>,
}

impl Subst {
  /// Combine stores and produce a substitution to apply to other things.
  pub fn get(this: &mut GlobalStore, other: LocalStore) -> Self {
    let mut ret = Subst { old_to_new: FxHashMap::default() };
    let orig_len = this.0.idx_to_data.len();
    for (data, mut old_ty) in other.0.data_to_idx {
      always!(!old_ty.is_local());
      old_ty.make_local();
      always!(!this.0.data_to_idx.contains_key(&data));
      let new_ty = Ty::from_idx(this.0.idx_to_data.len());
      this.0.idx_to_data.push(data.clone());
      always!(this.0.data_to_idx.insert(data, new_ty).is_none());
      ret.old_to_new.insert(old_ty, new_ty);
    }
    for data in &mut this.0.idx_to_data[orig_len..] {
      data.apply(&ret);
    }
    ret
  }
}
