//! Jsonnet values.

use jsonnet_expr::{Expr, Id, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Default, Clone)]
pub struct Env {
  store: FxHashMap<Id, (Env, Expr)>,
}

impl Env {
  pub fn insert(&mut self, id: Id, env: Env, expr: Expr) {
    self.store.insert(id, (env, expr));
  }

  #[must_use]
  pub fn get(&self, id: Id) -> (&Env, Expr) {
    let (ref env, expr) = self.store[&id];
    (env, expr)
  }
}

/// A Jsonnet value.
///
/// The spec uses eager substitution but I suspect this is prohibitively non-performant. So we
/// separate values into primitives and recursive values. Recursive values contain expressions,
/// because Jsonnet itself has lazy semantics.
///
/// Because of this, and also because we choose to implement substitution lazily (as opposed to the
/// spec which expresses the semantics with eager substitution), we must therefore also carry with
/// recursive values an environment in which to do lazy substitutions.
///
/// Note that implementing substitution lazily is not meant to break with the spec. The execution
/// should be semantically equivalent.
#[derive(Debug, Clone)]
pub enum Val {
  Prim(Prim),
  Object(Object),
  Array(Array),
  Function {
    env: Env,
    /// we'd like to get good performance for lookup by both index for positional arguments and name
    /// for keyword arguments, but to do that we'd need to something like double the memory and
    /// store both a vec and a map. which we could do but we choose to not right now.
    params: Vec<(Id, Expr)>,
    body: Expr,
  },
  StdFn(StdFn),
}

#[derive(Debug, Default, Clone)]
pub struct Object {
  parent: Option<Box<Object>>,
  env: Env,
  asserts: Vec<Expr>,
  fields: FxHashMap<Str, (Visibility, Expr)>,
}

impl Object {
  /// first itself, then its parent, etc
  fn ancestry(&self) -> impl Iterator<Item = &Self> {
    let mut cur = Some(self);
    std::iter::from_fn(move || {
      let this = cur?;
      cur = this.parent.as_deref();
      Some(this)
    })
  }

  pub(crate) fn parent(&self) -> Option<&Self> {
    self.ancestry().nth(1)
  }

  pub(crate) fn root(&self) -> Option<&Self> {
    self.ancestry().last()
  }

  #[must_use]
  pub(crate) fn new(
    env: Env,
    asserts: Vec<Expr>,
    fields: FxHashMap<Str, (Visibility, Expr)>,
  ) -> Object {
    Object { parent: None, env, asserts, fields }
  }

  pub(crate) fn asserts(&self) -> impl Iterator<Item = (&Env, Expr)> {
    self.ancestry().flat_map(|this| this.asserts.iter()).map(|&e| (&self.env, e))
  }

  pub(crate) fn fields(&self) -> impl Iterator<Item = (&Env, &Str, Visibility, Expr)> {
    let mut seen = FxHashSet::<&Str>::default();
    self
      .ancestry()
      .flat_map(|this| this.fields.iter())
      .map(|(name, &(vis, expr))| (&self.env, name, vis, expr))
      .filter(move |&(_, name, _, _)| seen.insert(name))
  }

  #[must_use]
  pub(crate) fn get_field(&self, name: &Str) -> Option<(&Env, Visibility, Expr)> {
    self
      .ancestry()
      .find_map(|this| this.fields.get(name))
      .map(move |&(vis, expr)| (&self.env, vis, expr))
  }

  pub(crate) fn set_parent_to(&mut self, other: Self) {
    let mut top = self;
    while let Some(ref mut parent) = top.parent {
      top = parent.as_mut();
    }
    top.parent = Some(Box::new(other));
  }
}

#[derive(Debug, Default, Clone)]
pub struct Array {
  /// arranging it in this way allows for different elements of the array to be lazy under different
  /// environments. this allows us to implement append
  parts: Vec<ArrayPart>,
}

impl Array {
  #[must_use]
  pub(crate) fn new(env: Env, elems: Vec<Expr>) -> Self {
    Self { parts: vec![ArrayPart { env, elems }] }
  }

  pub(crate) fn iter(&self) -> impl Iterator<Item = (&Env, Expr)> {
    self.parts.iter().flat_map(|part| part.elems.iter().map(|&elem| (&part.env, elem)))
  }

  #[must_use]
  pub(crate) fn get(&self, mut idx: usize) -> Option<(&Env, Expr)> {
    for part in &self.parts {
      match part.elems.get(idx) {
        Some(&elem) => return Some((&part.env, elem)),
        None => idx -= part.elems.len(),
      }
    }
    None
  }

  pub(crate) fn append(&mut self, other: &mut Self) {
    self.parts.append(&mut other.parts);
  }
}

#[derive(Debug, Clone)]
struct ArrayPart {
  env: Env,
  elems: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum StdFn {
  Cmp,
  Equals,
}
