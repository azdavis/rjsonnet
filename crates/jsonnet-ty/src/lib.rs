//! A type system for Jsonnet.

pub mod display;
pub mod logic;

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
pub enum Data {
  /// A primitive type.
  Prim(Prim),
  /// An array of elements, where each element has the given type.
  Array(Array),
  /// An object, possibly with known and unknown fields.
  Object(Object),
  /// A function type.
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
      Data::Array(arr) => arr.apply(subst),
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
      Data::Array(arr) => arr.has_local(),
      Data::Object(object) => object.has_local(),
      Data::Fn(f) => f.has_local(),
      Data::Union(parts) => parts.iter().any(|x| x.is_local()),
    }
  }

  /// Helper function to make a union type.
  #[must_use]
  pub fn mk_union<const N: usize>(tys: [Ty; N]) -> Self {
    Self::Union(Union::from(tys))
  }
}

/// A primitive type, containing no recursive data inside.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Prim {
  /// Anything at all.
  ///
  /// This is like `any` in TypeScript and `T.untyped` in Sorbet (Ruby type-checker). It is BOTH a
  /// "top type" and a "bottom type":
  ///
  /// - Anything can be coerced to it (top type)
  /// - It can be coerced to anything (bottom type)
  ///
  /// In that sense, it is the ultimate escape-hatch type. Its presence in the type system means the
  /// type system is not sound.
  ///
  /// But that's ok: the goal of the type system is mainly to show useful information **when we have
  /// that information** (from inference, etc), and give up and fall back to this "type" when we
  /// don't know.
  ///
  /// If the goal was to ensure soundness, we'd probably have to do at least one of:
  ///
  /// - Demand way more type annotations from the source language. This is more disruptive to the
  ///   user and thus not amenable to incremental adoption.
  /// - Have a much more sophisticated inference algorithm. This would be more burden on
  ///   implementors. It is also impossible by Rice's theorem to completely analyze arbitrary
  ///   jsonnet (or any language) and emit type errors **if and only if** a type error would happen
  ///   at runtime.
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

/// An array type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Array {
  /// The type of elements in the array.
  pub elem: Ty,
  /// Whether this array is a set, i.e. known at runtime to be sorted and duplicate-free.
  ///
  /// We treat regular array types and "set" array types mostly the same in the type system, but we
  /// do use this info in a few places, primarily to make sure the "set operation" std functions
  /// only operate on known sets.
  pub is_set: bool,
}

impl Array {
  /// Returns a new (non-set) array type.
  #[must_use]
  pub fn new(elem: Ty) -> Self {
    Self { elem, is_set: false }
  }

  /// Returns a new "set" array type.
  #[must_use]
  pub fn set(elem: Ty) -> Self {
    Self { elem, is_set: true }
  }

  fn apply(&mut self, subst: &Subst) {
    self.elem.apply(subst);
  }

  fn has_local(self) -> bool {
    self.elem.is_local()
  }
}

/// An object type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Object {
  /// The known fields.
  pub known: BTreeMap<Str, Ty>,
  /// Whether this has unknown (dynamic) fields. For example, this guy does:
  /// ```jsonnet
  /// { [std.extVar("SHELL")]: "hi" }
  /// ```
  pub has_unknown: bool,
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

  /// Returns a totally empty object that is known to be empty. It has no known OR unknown fields.
  #[must_use]
  pub fn empty() -> Self {
    Self { known: BTreeMap::new(), has_unknown: false }
  }

  /// Returns an object with no known fields, but may have unknown fields.
  #[must_use]
  pub fn unknown() -> Self {
    Self { known: BTreeMap::new(), has_unknown: true }
  }
}

/// A function type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Fn {
  /// A regular, user-written function, with some arguments and a return type.
  Regular(RegularFn),
  /// A standard library function.
  Std(jsonnet_expr::StdFn),
  /// A function known to never have named params or optional params, only a given number of
  /// required positional params. The params and return type are not known (aka any).
  ///
  /// Only used as the type of parameters in std fns, not writeable in user code.
  StdParam(ParamCount),
  /// A function for which the number of params or return type is not known at all. This is the type
  /// of locals that are checked with `std.isFunction`.
  Unknown,
}

impl Fn {
  fn apply(&mut self, subst: &Subst) {
    match self {
      Fn::Regular(f) => f.apply(subst),
      Fn::Std(_) | Fn::StdParam(_) | Fn::Unknown => {}
    }
  }

  fn has_local(&self) -> bool {
    match self {
      Fn::Regular(f) => f.has_local(),
      Fn::Std(_) | Fn::StdParam(_) | Fn::Unknown => false,
    }
  }

  /// Returns the params and return type for this, or `None` if this is `Unknown`.
  #[must_use]
  pub fn parts(&self) -> Option<(&[Param], Ty)> {
    match self {
      Fn::Regular(func) => Some((func.params.as_slice(), func.ret)),
      Fn::Std(func) => {
        let sig = StdFnSig::get(*func);
        Some((sig.params, sig.ret))
      }
      Fn::StdParam(param_count) => {
        let params = match param_count {
          ParamCount::One => [Param::A].as_slice(),
          ParamCount::Two => [Param::A, Param::B].as_slice(),
        };
        Some((params, Ty::ANY))
      }
      Fn::Unknown => None,
    }
  }
}

/// A number of params a std param fn has.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ParamCount {
  /// 1 param.
  One,
  /// 2 params.
  Two,
}

impl ParamCount {
  /// Converts this to a usize.
  #[must_use]
  pub fn to_usize(self) -> usize {
    match self {
      ParamCount::One => 1,
      ParamCount::Two => 2,
    }
  }
}

/// A regular function type, the type of a user-written function.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RegularFn {
  /// The parameters.
  pub params: Vec<Param>,
  /// The return type.
  pub ret: Ty,
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
pub struct Param {
  /// The name of it.
  pub id: Id,
  /// Its type.
  pub ty: Ty,
  /// Whether it is required.
  pub required: bool,
}

impl Param {
  const A: Self = Self::required_any(Id::a_unutterable);
  const B: Self = Self::required_any(Id::b_unutterable);
  const UNUTTERABLE: [Self; 5] = [
    Self::A,
    Self::B,
    Self::required_any(Id::c_unutterable),
    Self::required_any(Id::d_unutterable),
    Self::required_any(Id::e_unutterable),
  ];

  const fn required_any(id: Id) -> Self {
    Self { id, ty: Ty::ANY, required: true }
  }

  fn apply(&mut self, subst: &Subst) {
    self.ty.apply(subst);
  }

  fn has_local(&self) -> bool {
    self.ty.is_local()
  }
}

/// A union of types.
pub type Union = BTreeSet<Ty>;

/// A type.
///
/// Internally represented as an index that is cheap to copy.
///
/// Types are unique. That is, for two types a and b, if a != b, then a's data != b's data.
///
/// BUT NOTE that the SEMANTICS of a type can be the same as another type but the types are
/// different, like:
///
/// - `{ a: int } | { a: string }`
/// - `{ a: int | string }`
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
pub struct MutStore<'a> {
  global: &'a GlobalStore,
  local: LocalStore,
}

impl<'a> MutStore<'a> {
  /// Returns a new one based off a `GlobalStore`.
  #[must_use]
  pub fn new(global: &'a GlobalStore) -> Self {
    Self { global, local: LocalStore::default() }
  }

  /// Get the `Ty` for the data. If one existed already in this, return that. Else create a new `Ty`
  /// for it.
  pub fn get(&mut self, data: Data) -> Ty {
    match data {
      // micro optimizations (maybe). deferring to get_inner would also be correct
      Data::Prim(prim) => match prim {
        Prim::Any => Ty::ANY,
        Prim::True => Ty::TRUE,
        Prim::False => Ty::FALSE,
        Prim::Null => Ty::NULL,
        Prim::String => Ty::STRING,
        Prim::Number => Ty::NUMBER,
      },
      Data::Fn(Fn::Std(f)) => Ty::std_fn(f),
      Data::Fn(Fn::Unknown) => Ty::FUNCTION,
      // go directly to get inner.
      Data::Array(_) | Data::Object(_) | Data::Fn(Fn::Regular(_) | Fn::StdParam(_)) => {
        self.get_inner(data)
      }
      // the interesting case.
      Data::Union(work) => {
        let mut work: Vec<_> = work.into_iter().collect();
        let mut parts = Union::new();
        while let Some(ty) = work.pop() {
          match self.data(ty) {
            Data::Prim(Prim::Any) => return Ty::ANY,
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

  /// Returns the data for the `Ty`.
  #[must_use]
  pub fn data(&self, ty: Ty) -> &Data {
    let (idx, is_local) = ty.to_data();
    let store = if is_local { &self.local.0 } else { &self.global.0 };
    match store.idx_to_data.get(idx) {
      None => {
        always!(false, "should be able to get data for {ty:?}");
        &Data::Prim(Prim::Any)
      }
      Some(x) => x,
    }
  }

  /// Turns this into a `LocalStore`, which holds all the `Ty`s we created in this via calls to
  /// `get` that mutated this.
  #[must_use]
  pub fn into_local(self) -> LocalStore {
    self.local
  }

  fn object_fields(&mut self, ty: Ty, ac: &mut BTreeMap<Str, Ty>) -> bool {
    match self.data(ty) {
      Data::Prim(_) | Data::Array(_) | Data::Fn(_) => false,
      Data::Object(object) => {
        for (field, ty) in object.clone().known {
          let cur = ac.entry(field).or_insert(Ty::NEVER);
          *cur = self.get(Data::mk_union([*cur, ty]));
        }
        true
      }
      Data::Union(tys) => tys.clone().into_iter().all(|ty| self.object_fields(ty, ac)),
    }
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

impl GlobalStore {
  /// If this type is an object type or a union of object types, returns a mapping from field names
  /// to types for the known fields of this type.
  #[must_use]
  pub fn object_fields(&mut self, ty: Ty) -> Option<BTreeMap<Str, Ty>> {
    let mut ac = BTreeMap::<Str, Ty>::new();
    let mut m = MutStore::new(&*self);
    let ok = m.object_fields(ty, &mut ac);
    let local = m.into_local();
    always!(Subst::get(self, local).is_none());
    ok.then_some(ac)
  }

  /// Returns Some(f) iff this is a function type f.
  #[must_use]
  pub fn as_fn(&self, ty: Ty) -> Option<&Fn> {
    if let Data::Fn(f) = self.0.data(ty, false)? {
      Some(f)
    } else {
      None
    }
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
#[derive(Debug, Default)]
pub struct Subst {
  /// INVARIANT: map from local to non-local tys
  old_to_new: FxHashMap<Ty, Ty>,
}

impl Subst {
  /// Combines the local store into the global one. Then, if that combination produced a non-empty
  /// substitution to apply to other things, return it.
  ///
  /// # Panics
  ///
  /// On internal error in debug mode only.
  pub fn get(global: &mut GlobalStore, mut local: LocalStore) -> Option<Self> {
    // topological sort to determine what order to add the data from the local store to the global
    // store.
    let mut work: Vec<_> = (0..local.0.idx_to_data.len())
      .map(|idx| {
        let mut t = Ty::from_idx(idx);
        t.make_local();
        TopoSortAction::start(t)
      })
      .collect();
    let mut cur = FxHashSet::<Ty>::default();
    let mut done = FxHashSet::<Ty>::default();
    let mut order = Vec::<Ty>::default();
    let mut saw_cycle = false;
    while let Some(TopoSortAction(ty, kind)) = work.pop() {
      match kind {
        TopoSortActionKind::Start => {
          if done.contains(&ty) {
            continue;
          }
          let Some(data) = local.0.data(ty, true) else { continue };
          if !cur.insert(ty) {
            always!(false, "cycle with {ty:?}");
            saw_cycle = true;
            continue;
          }
          work.push(TopoSortAction::end(ty));
          match data {
            // known to all already exist in the global store.
            Data::Prim(_) | Data::Fn(Fn::Std(_) | Fn::StdParam(_) | Fn::Unknown) => {}
            Data::Array(arr) => work.push(TopoSortAction::start(arr.elem)),
            Data::Object(object) => {
              let iter = object.known.values().map(|&t| TopoSortAction::start(t));
              work.extend(iter);
            }
            Data::Fn(Fn::Regular(func)) => {
              let params = func.params.iter().map(|x| x.ty);
              let iter = params.chain(std::iter::once(func.ret)).map(TopoSortAction::start);
              work.extend(iter);
            }
            Data::Union(tys) => {
              let iter = tys.iter().map(|&t| TopoSortAction::start(t));
              work.extend(iter);
            }
          }
        }
        TopoSortActionKind::End => {
          always!(ty.is_local());
          always!(cur.remove(&ty));
          always!(done.insert(ty));
          order.push(ty);
        }
      }
    }
    // make a few checks, drop some intermediate values, then proceed to build the subst in the
    // topologically sorted order.
    //
    // the important thing is that for a type T in the order, for all types U in data(T), U precedes
    // T in the order.
    always!(cur.is_empty() != saw_cycle);
    always!(done.len() == order.len());
    drop(work);
    drop(cur);
    drop(done);
    let mut ret = Subst::default();
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
    // some final consistency checks.
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
    (!ret.is_empty()).then_some(ret)
  }

  fn is_empty(&self) -> bool {
    self.old_to_new.is_empty()
  }
}

#[derive(Debug)]
enum TopoSortActionKind {
  Start,
  End,
}

#[derive(Debug)]
struct TopoSortAction(Ty, TopoSortActionKind);

impl TopoSortAction {
  const fn start(t: Ty) -> Self {
    Self(t, TopoSortActionKind::Start)
  }

  const fn end(t: Ty) -> Self {
    Self(t, TopoSortActionKind::End)
  }
}

/// A signature for a standard library function.
#[derive(Debug)]
pub struct StdFnSig {
  /// The params
  pub params: &'static [Param],
  /// The return type.
  pub ret: Ty,
}
