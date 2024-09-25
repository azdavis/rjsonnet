//! Jsonnet values.

use crate::error::Cycle;
use jsonnet_expr::{Expr, Id, Prim, StdFn, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;

#[derive(Debug, Clone)]
pub(crate) struct Env {
  /// TODO make priv?
  pub(crate) path: paths::PathId,
  store: FxHashMap<Id, (Env, Expr)>,
  cur: Vec<paths::PathId>,
  this: Option<Box<Object>>,
}

impl Env {
  pub(crate) fn add_binds(&self, binds: &[(Id, Expr)]) -> Env {
    let mut ret = self.clone();
    for &(bind, expr) in binds {
      ret.insert(bind, self.clone(), expr);
    }
    ret
  }

  pub(crate) fn empty(path: paths::PathId) -> Self {
    Self { path, store: FxHashMap::default(), cur: Vec::new(), this: None }
  }

  pub(crate) fn empty_with_cur(&self, path: paths::PathId) -> Result<Self, Cycle> {
    let mut cur = self.cur.clone();
    let idx = self.cur.iter().position(|&p| p == path);
    match idx {
      None => {
        cur.push(path);
        Ok(Self { path, store: FxHashMap::default(), cur, this: None })
      }
      Some(idx) => Err(Cycle { first_and_last: path, intervening: cur.split_off(idx + 1) }),
    }
  }

  pub(crate) fn insert(&mut self, id: Id, env: Env, expr: Expr) {
    self.store.insert(id, (env, expr));
  }

  #[must_use]
  pub(crate) fn get(&self, id: Id) -> Option<Get<'_>> {
    if id == Id::self_ {
      return Some(Get::Self_);
    }
    if id == Id::super_ {
      return Some(Get::Super);
    }
    if id == Id::std_unutterable {
      return Some(Get::Std);
    }
    if let Some(&(ref env, expr)) = self.store.get(&id) {
      Some(Get::Expr(env, expr))
    } else if id == Id::std {
      Some(Get::Std)
    } else {
      None
    }
  }

  #[must_use]
  pub(crate) fn this(&self) -> Option<&Object> {
    self.this.as_deref()
  }
}

pub(crate) enum Get<'a> {
  Self_,
  Super,
  Std,
  Expr(&'a Env, Expr),
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
pub(crate) enum Val {
  Prim(Prim),
  Object(Object),
  Array(Array),
  Fn(Fn),
}

#[derive(Debug, Clone)]
pub(crate) struct Object {
  parent: Option<Box<Object>>,
  kind: ObjectKind,
  /// skip fields directly on this. used to implement super. kind of strange. maybe we could
  /// rearrange this to take better advantage of the fact that we know that `super` must always be
  /// immediately followed by a field-get (super.foo or super[foo] or foo in super)?
  is_super: bool,
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

  /// same as [`ancestry`], but without itself if this is super.
  fn ancestry_considering_superness(&self) -> impl Iterator<Item = &Self> {
    // skip itself (the first one) if this is super. boolean converts to 1 if true, 0 if false.
    self.ancestry().skip(self.is_super.into())
  }

  #[must_use]
  pub(crate) fn new(
    env: Env,
    binds: Vec<(Id, Expr)>,
    asserts: Vec<Expr>,
    fields: BTreeMap<Str, (Visibility, Expr)>,
  ) -> Self {
    let kind = ObjectKind::Regular(RegularObjectKind { env, binds, asserts, fields });
    Self { parent: None, kind, is_super: false }
  }

  #[must_use]
  pub(crate) fn std_lib() -> Self {
    Self { parent: None, kind: ObjectKind::Std, is_super: false }
  }

  pub(crate) fn parent(&self) -> Self {
    let mut parent = self.clone();
    parent.is_super = true;
    parent
  }

  fn set_this(&self, binds: &[(Id, Expr)], env: &Env) -> Env {
    let mut env = env.clone();
    let mut this = self.clone();
    this.is_super = false;
    env.this = Some(Box::new(this));
    env.add_binds(binds)
  }

  pub(crate) fn asserts(&self) -> impl Iterator<Item = (Env, Expr)> + '_ {
    let iter = self
      .ancestry()
      .filter_map(|this| match &this.kind {
        ObjectKind::Regular(this) => Some(this),
        ObjectKind::Std => None,
      })
      .flat_map(|this| this.asserts.iter().map(move |&expr| (this, expr)));
    iter.map(|(this, expr)| (self.set_this(&this.binds, &this.env), expr))
  }

  /// TODO this should be a generator
  pub(crate) fn fields(&self) -> Vec<(Str, Visibility, Field)> {
    let mut ret = Vec::<(Str, Visibility, Field)>::new();
    let mut seen = FxHashSet::<Str>::default();
    for this in self.ancestry_considering_superness() {
      match &this.kind {
        ObjectKind::Regular(this) => {
          for (name, &(vis, expr)) in &this.fields {
            if !seen.insert(name.clone()) {
              continue;
            }
            ret.push((name.clone(), vis, Field::Expr(self.set_this(&this.binds, &this.env), expr)));
          }
        }
        ObjectKind::Std => {
          for (name, field) in StdField::all() {
            if !seen.insert(name.clone()) {
              continue;
            }
            ret.push((name, Visibility::Hidden, Field::Std(field)));
          }
        }
      }
    }
    ret
  }

  #[must_use]
  pub(crate) fn get_field(&self, name: &Str) -> Option<(Visibility, Field)> {
    self.ancestry_considering_superness().find_map(|this| match &this.kind {
      ObjectKind::Std => {
        let field = StdField::try_from(name).ok()?;
        Some((Visibility::Hidden, Field::Std(field)))
      }
      ObjectKind::Regular(this) => {
        let &(vis, expr) = this.fields.get(name)?;
        Some((vis, Field::Expr(self.set_this(&this.binds, &this.env), expr)))
      }
    })
  }

  pub(crate) fn set_parent_to(&mut self, other: Self) {
    let mut top = self;
    while let Some(ref mut parent) = top.parent {
      top = parent.as_mut();
    }
    top.parent = Some(Box::new(other));
  }
}

#[derive(Debug, Clone)]
enum ObjectKind {
  Std,
  Regular(RegularObjectKind),
}

#[derive(Debug, Clone)]
struct RegularObjectKind {
  env: Env,
  /// this is annoying. but we put these here separately because we need to set the new `this` env
  /// before evaluating these binds. probably this setup could be improved.
  binds: Vec<(Id, Expr)>,
  asserts: Vec<Expr>,
  /// we want non-random order
  fields: BTreeMap<Str, (Visibility, Expr)>,
}

#[derive(Debug)]
pub(crate) enum Field {
  Std(StdField),
  Expr(Env, Expr),
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum StdField {
  ThisFile,
  Fn(StdFn),
}

impl StdField {
  fn all() -> impl Iterator<Item = (Str, Self)> {
    let it = StdFn::ALL.into_iter().map(|(a, b)| (a, StdField::Fn(b)));
    std::iter::once((Str::thisFile, StdField::ThisFile)).chain(it)
  }
}

impl TryFrom<&Str> for StdField {
  type Error = ();

  fn try_from(s: &Str) -> Result<Self, Self::Error> {
    if *s == Str::thisFile {
      return Ok(Self::ThisFile);
    }
    if let Some(&(_, x)) = StdFn::ALL.iter().find(|&(x, _)| x == s) {
      return Ok(Self::Fn(x));
    }
    Err(())
  }
}

#[derive(Debug, Default, Clone)]
pub(crate) struct Array {
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

  pub(crate) fn len(&self) -> usize {
    self.parts.iter().map(|x| x.elems.len()).sum()
  }
}

#[derive(Debug, Clone)]
struct ArrayPart {
  env: Env,
  elems: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub(crate) enum Fn {
  Regular(RegularFn),
  Std(StdFn),
}

#[derive(Debug, Clone)]
pub(crate) struct RegularFn {
  pub(crate) env: Env,
  /// we'd like to get good performance for lookup by both index for positional arguments and name
  /// for keyword arguments, but to do that we'd need to something like double the memory and
  /// store both a vec and a map. which we could do but we choose to not right now.
  pub(crate) params: Vec<(Id, Option<Expr>)>,
  pub(crate) body: Expr,
}
