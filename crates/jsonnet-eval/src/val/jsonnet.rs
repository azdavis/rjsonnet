//! Jsonnet values.

use jsonnet_expr::{Expr, Id, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Default, Clone)]
pub struct Env {
  store: FxHashMap<Id, Subst>,
}

impl Env {
  pub fn insert(&mut self, id: Id, subst: Subst) {
    self.store.insert(id, subst);
  }

  #[must_use]
  pub fn get(&self, id: Id) -> &Subst {
    &self.store[&id]
  }
}

#[derive(Debug, Clone)]
pub enum Subst {
  Val(Val),
  Expr(Env, Expr),
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
  parts: Vec<ObjectPart>,
}

impl Object {
  #[must_use]
  pub(crate) fn new(
    env: Env,
    asserts: Vec<Expr>,
    fields: FxHashMap<Str, (Visibility, Expr)>,
  ) -> Object {
    Self { parts: vec![ObjectPart { env, asserts, fields }] }
  }

  pub(crate) fn insert_self_super(&mut self) {
    let this = self.clone();
    for part in &mut self.parts {
      part.env.insert(Id::SELF, Subst::Val(Val::Object(this.clone())));
      part.env.insert(Id::SUPER, Subst::Val(Val::Object(Object::default())));
    }
  }

  pub(crate) fn asserts(&self) -> impl Iterator<Item = (&Env, Expr)> {
    self.parts.iter().flat_map(|part| part.asserts.iter().map(|&e| (&part.env, e)))
  }

  pub(crate) fn fields(&self) -> impl Iterator<Item = (&Env, &Str, Visibility, Expr)> {
    let mut seen = FxHashSet::<&Str>::default();
    self
      .parts
      .iter()
      .flat_map(|part| part.fields.iter().map(|(name, &(vis, expr))| (&part.env, name, vis, expr)))
      .filter(move |&(_, name, _, _)| seen.insert(name))
  }

  #[must_use]
  pub(crate) fn get_field(&self, name: &Str) -> Option<(&Env, Visibility, Expr)> {
    self
      .parts
      .iter()
      .find_map(|part| part.fields.get(name).map(|&(vis, expr)| (&part.env, vis, expr)))
  }
}

#[derive(Debug, Clone)]
struct ObjectPart {
  env: Env,
  asserts: Vec<Expr>,
  fields: FxHashMap<Str, (Visibility, Expr)>,
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
