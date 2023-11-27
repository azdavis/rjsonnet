//! Jsonnet values.

use jsonnet_expr::{Expr, Id, Prim, Str, Visibility};
use rustc_hash::FxHashMap;

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
  Object {
    env: Env,
    asserts: Vec<Expr>,
    fields: FxHashMap<Str, (Visibility, Expr)>,
  },
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

impl Val {
  #[must_use]
  pub fn empty_object() -> Self {
    Self::Object { env: Env::default(), asserts: Vec::new(), fields: FxHashMap::default() }
  }
}

#[derive(Debug, Clone)]
pub struct Array {
  /// arranging it in this way allows for different elements of the array to be lazy under different
  /// environments. this allows us to implement append
  parts: Vec<ArrayPart>,
}

impl Array {
  #[must_use]
  pub fn new(env: Env, elems: Vec<Expr>) -> Self {
    Self { parts: vec![ArrayPart { env, elems }] }
  }

  pub fn iter(&self) -> impl Iterator<Item = (&Env, Expr)> {
    self.parts.iter().flat_map(|part| part.elems.iter().map(|&elem| (&part.env, elem)))
  }

  #[must_use]
  pub fn get(&self, mut idx: usize) -> Option<(&Env, Expr)> {
    for part in &self.parts {
      match part.elems.get(idx) {
        Some(&elem) => return Some((&part.env, elem)),
        None => idx -= part.elems.len(),
      }
    }
    None
  }

  pub fn append(&mut self, other: &mut Self) {
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
