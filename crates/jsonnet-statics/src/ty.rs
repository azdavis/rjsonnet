//! A rudimentary type system for Jsonnet.

pub mod display;

mod generated {
  include!(concat!(env!("OUT_DIR"), "/generated.rs"));
}

use always::{always, convert};
use jsonnet_expr::{ExprMust, Id, Str};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

/// A map from expr to type.
pub type Exprs = FxHashMap<ExprMust, Ty>;

/// Data about a type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Data {
  /// A primitive type.
  Prim(Prim),
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
      Data::Prim(_) => {}
    }
  }

  fn has_local(&self) -> bool {
    match self {
      Data::Prim(_) => false,
      Data::Array(ty) => ty.is_local(),
      Data::Object(object) => object.has_local(),
      Data::Fn(f) => f.has_local(),
      Data::Union(parts) => parts.iter().any(|x| x.is_local()),
    }
  }
}

/// A primitive type, containing no recursive data inside.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Prim {
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

  fn has_local(&self) -> bool {
    self.known.values().any(|x| x.is_local())
  }

  pub(crate) fn empty() -> Self {
    Self { known: BTreeMap::new(), has_unknown: false }
  }

  pub(crate) fn unknown() -> Self {
    Self { known: BTreeMap::new(), has_unknown: true }
  }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum Fn {
  Regular(RegularFn),
  Std(jsonnet_expr::StdFn),
}

impl Fn {
  fn apply(&mut self, subst: &Subst) {
    match self {
      Fn::Regular(f) => f.apply(subst),
      Fn::Std(_) => {}
    }
  }

  fn has_local(&self) -> bool {
    match self {
      Fn::Regular(f) => f.has_local(),
      Fn::Std(_) => false,
    }
  }
}

/// A function type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct RegularFn {
  /// The parameters.
  pub(crate) params: Vec<Param>,
  /// The return type.
  pub(crate) ret: Ty,
}

impl RegularFn {
  fn apply(&mut self, subst: &Subst) {
    for param in &mut self.params {
      param.apply(subst);
    }
    self.ret.apply(subst);
  }

  fn has_local(&self) -> bool {
    self.params.iter().any(Param::has_local) || self.ret.is_local()
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

  fn has_local(&self) -> bool {
    self.ty.is_local()
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
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Ty(u32);

impl fmt::Debug for Ty {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let (idx, is_local) = self.to_data();
    f.debug_struct("Ty").field("idx", &idx).field("is_local", &is_local).finish()
  }
}

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

  /// returns a tuple of (index, is local)
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
        Some(&Data::Prim(Prim::Any))
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
      // micro optimization (maybe). deferring to get_inner would also be correct
      Data::Prim(prim) => match prim {
        Prim::Any => Ty::ANY,
        Prim::True => Ty::TRUE,
        Prim::False => Ty::FALSE,
        Prim::Null => Ty::NULL,
        Prim::String => Ty::STRING,
        Prim::Number => Ty::NUMBER,
      },
      // another one.
      Data::Fn(Fn::Std(f)) => Ty::std_fn(f),
      // go directly to get inner.
      Data::Array(_) | Data::Object(_) | Data::Fn(Fn::Regular(_)) => self.get_inner(data),
      // the interesting case.
      Data::Union(work) => {
        let mut work: Vec<_> = work.into_iter().collect();
        let mut parts = BTreeSet::<Ty>::new();
        while let Some(ty) = work.pop() {
          match self.global.0.data(ty, false) {
            Some(Data::Prim(Prim::Any)) => return Ty::ANY,
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
        &Data::Prim(Prim::Any)
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
  /// INVARIANT: map from local to non-local tys
  old_to_new: FxHashMap<Ty, Ty>,
}

impl Subst {
  /// Combine stores and produce a substitution to apply to other things.
  ///
  /// # Panics
  ///
  /// On internal error in debug mode only.
  pub fn get(global: &mut GlobalStore, mut local: LocalStore) -> Self {
    // topological sort to determine what order to add the data from the local store to the global
    // store.
    let mut work: Vec<_> = (0..local.0.idx_to_data.len())
      .map(|idx| {
        let mut t = Ty::from_idx(idx);
        t.make_local();
        Action::start(t)
      })
      .collect();
    let mut cur = FxHashSet::<Ty>::default();
    let mut done = FxHashSet::<Ty>::default();
    let mut order = Vec::<Ty>::default();
    let mut saw_cycle = false;
    while let Some(Action(ty, kind)) = work.pop() {
      match kind {
        ActionKind::Start => {
          if done.contains(&ty) {
            continue;
          }
          let Some(data) = local.0.data(ty, true) else { continue };
          if !cur.insert(ty) {
            always!(false, "cycle with {ty:?}");
            saw_cycle = true;
            continue;
          }
          work.push(Action::end(ty));
          match data {
            Data::Prim(_) => {}
            Data::Array(ty) => work.push(Action::start(*ty)),
            Data::Object(object) => {
              let iter = object.known.values().map(|&t| Action::start(t));
              work.extend(iter);
            }
            Data::Fn(func) => match func {
              Fn::Regular(func) => {
                let params = func.params.iter().map(|x| x.ty);
                let iter = params.chain(std::iter::once(func.ret)).map(Action::start);
                work.extend(iter);
              }
              Fn::Std(_) => {}
            },
            Data::Union(tys) => {
              let iter = tys.iter().map(|&t| Action::start(t));
              work.extend(iter);
            }
          }
        }
        ActionKind::End => {
          always!(ty.is_local());
          always!(cur.remove(&ty));
          always!(done.insert(ty));
          order.push(ty);
        }
      }
    }
    always!(cur.is_empty() != saw_cycle);
    always!(done.len() == order.len());
    drop(work);
    drop(cur);
    drop(done);
    let mut ret = Subst { old_to_new: FxHashMap::default() };
    for old in order {
      let (idx, is_local) = old.to_data();
      always!(is_local);
      let Some(data) = local.0.idx_to_data.get_mut(idx) else {
        always!(
          false,
          "should be able to index into idx_to_data with a ty that came from that len: {idx:?}"
        );
        continue;
      };
      // take the data out without cloning and replace it with something inert.
      always!(!matches!(*data, Data::Prim(_)));
      let mut data = std::mem::replace(data, Data::Prim(Prim::Any));
      // apply the subst in progress to each data. since we topologically sorted, after applying,
      // the data should contain no local types. this might NOT be the case if we processed the
      // local data in an arbitrary order.
      data.apply(&ret);
      debug_assert!(!data.has_local());
      let new = match global.0.data_to_idx.entry(data) {
        Entry::Occupied(entry) => *entry.get(),
        Entry::Vacant(entry) => {
          let new = Ty::from_idx(global.0.idx_to_data.len());
          global.0.idx_to_data.push(entry.key().clone());
          *entry.insert(new)
        }
      };
      always!(!new.is_local());
      // in the expr subst, we check if old != new before inserting. we can save and avoid the
      // insertion if they are the same. however, here, they will ALWAYS be different because old is
      // local and new is not.
      always!(old != new);
      always!(ret.old_to_new.insert(old, new).is_none());
    }
    always!(global.0.data_to_idx.len() == global.0.idx_to_data.len());
    if cfg!(debug_assertions) {
      for (data, ty) in &global.0.data_to_idx {
        let (idx, is_local) = ty.to_data();
        assert!(!is_local);
        let other_data = &global.0.idx_to_data[idx];
        assert_eq!(data, other_data);
        assert!(!data.has_local());
      }
    }
    ret
  }
}

#[derive(Debug)]
enum ActionKind {
  Start,
  End,
}

#[derive(Debug)]
struct Action(Ty, ActionKind);

impl Action {
  const fn start(t: Ty) -> Self {
    Self(t, ActionKind::Start)
  }

  const fn end(t: Ty) -> Self {
    Self(t, ActionKind::End)
  }
}
