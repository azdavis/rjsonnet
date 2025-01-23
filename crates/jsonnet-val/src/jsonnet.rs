//! Jsonnet values.

use always::always;
use jsonnet_expr::{Expr, Id, Prim, StdField, StdFn, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};
use std::collections::BTreeMap;

/// An environment, which stores a mapping of identifiers to unevaluated expressions. Since the
/// expressions are unevaluated, they have with them their own envs to evaluate.
#[derive(Debug, Clone)]
pub struct Env {
  path: paths::PathId,
  store: FxHashMap<Id, (Env, Expr)>,
  cur: Vec<paths::PathId>,
  this: Option<Box<Object>>,
}

impl Env {
  /// Returns the path that this env came from.
  #[must_use]
  pub fn path(&self) -> paths::PathId {
    self.path
  }

  /// Returns a new env that is this env plus the binds.
  #[must_use]
  pub fn add_binds(&self, binds: &[(Id, Expr)]) -> Env {
    // TODO fix mutual recursion
    let mut ret = self.clone();
    for &(bind, expr) in binds {
      ret.insert(bind, self.clone(), expr);
    }
    ret
  }

  /// Returns an empty env.
  #[must_use]
  pub fn empty(path: paths::PathId) -> Self {
    Self { path, store: FxHashMap::default(), cur: Vec::new(), this: None }
  }

  /// Returns an empty env, but check to see if this would cause a cycle.
  ///
  /// # Errors
  ///
  /// If there would be a cycle.
  pub fn empty_with_cur(&self, path: paths::PathId) -> Result<Self, Cycle> {
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

  /// Insert an id-expr mapping.
  pub fn insert(&mut self, id: Id, env: Env, expr: Expr) {
    self.store.insert(id, (env, expr));
  }

  /// Get an identifier.
  #[must_use]
  pub fn get(&self, id: Id) -> Option<Get<'_>> {
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

  /// Returns what `self` refers to in this env.
  #[must_use]
  pub fn this(&self) -> Option<&Object> {
    self.this.as_deref()
  }
}

/// A cycle error.
#[derive(Debug, Clone)]
pub struct Cycle {
  /// The first and last thing in the cycle.
  pub first_and_last: paths::PathId,
  /// The other stuff in the cycle, in order.
  pub intervening: Vec<paths::PathId>,
}

/// The output when getting an identifier.
#[derive(Debug)]
pub enum Get<'a> {
  /// The id was `self`.
  Self_,
  /// The id was `super`.
  Super,
  /// The id was `std`, the standard library.
  Std,
  /// The id mapped to an expr.
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
pub enum Val {
  /// A primitive.
  Prim(Prim),
  /// A lazy object.
  Object(Object),
  /// A lazy array.
  Array(Array),
  /// A function.
  Fn(Fn),
}

impl From<bool> for Val {
  fn from(b: bool) -> Self {
    Val::Prim(Prim::Bool(b))
  }
}

impl From<Str> for Val {
  fn from(s: Str) -> Self {
    Val::Prim(Prim::String(s))
  }
}

impl From<finite_float::Float> for Val {
  fn from(n: finite_float::Float) -> Self {
    Val::Prim(Prim::Number(n))
  }
}

/// A lazy object, with an ancestry chain from `+`.
#[derive(Debug, Clone)]
pub struct Object {
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

  /// Returns a new regular (non-std) object.
  #[must_use]
  pub fn new(env: Env, asserts: Vec<Expr>, fields: BTreeMap<Str, (Visibility, Expr)>) -> Self {
    let kind = ObjectKind::Regular(RegularObjectKind { env, asserts, fields });
    Self { parent: None, kind, is_super: false }
  }

  /// Returns the standard library object.
  #[must_use]
  pub fn std_lib() -> Self {
    Self { parent: None, kind: ObjectKind::Std, is_super: false }
  }

  /// Returns the parent of this.
  #[must_use]
  pub fn parent(&self) -> Self {
    let mut parent = self.clone();
    parent.is_super = true;
    parent
  }

  fn set_this(&self, env: &Env) -> Env {
    let mut env = env.clone();
    let mut this = self.clone();
    this.is_super = false;
    env.this = Some(Box::new(this));
    env
  }

  /// Returns the asserts in this.
  pub fn asserts(&self) -> impl Iterator<Item = (Env, Expr)> + use<'_> {
    let iter = self
      .ancestry()
      .filter_map(|this| match &this.kind {
        ObjectKind::Regular(this) => Some(this),
        ObjectKind::Std => None,
      })
      .flat_map(|this| this.asserts.iter().map(move |&expr| (this, expr)));
    iter.map(|(this, expr)| (self.set_this(&this.env), expr))
  }

  /// TODO this should be a generator
  #[must_use]
  pub fn fields(&self) -> Vec<(Str, Visibility, Field)> {
    let mut ret = Vec::<(Str, Visibility, Field)>::new();
    let mut seen = FxHashSet::<Str>::default();
    for this in self.ancestry_considering_superness() {
      match &this.kind {
        ObjectKind::Regular(this) => {
          for (name, &(vis, expr)) in &this.fields {
            if !seen.insert(name.clone()) {
              continue;
            }
            ret.push((name.clone(), vis, Field::Expr(self.set_this(&this.env), expr)));
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

  /// Gets a field off an object.
  #[must_use]
  pub fn get_field(&self, name: &Str) -> Option<(Visibility, Field)> {
    self.ancestry_considering_superness().find_map(|this| match &this.kind {
      ObjectKind::Std => {
        let field = StdField::try_from(name).ok()?;
        Some((Visibility::Hidden, Field::Std(field)))
      }
      ObjectKind::Regular(this) => {
        let &(vis, expr) = this.fields.get(name)?;
        Some((vis, Field::Expr(self.set_this(&this.env), expr)))
      }
    })
  }

  /// Set this object's parent.
  pub fn set_parent_to(&mut self, other: Self) {
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
  asserts: Vec<Expr>,
  /// we want non-random order
  fields: BTreeMap<Str, (Visibility, Expr)>,
}

/// A field of an object.
#[derive(Debug)]
pub enum Field {
  /// A standard library field.
  Std(StdField),
  /// A regular expression field.
  Expr(Env, Expr),
}

/// A lazy array.
#[derive(Debug, Default, Clone)]
pub struct Array {
  /// arranging it in this way allows for different elements of the array to be lazy under different
  /// environments. this allows us to implement append and `+`
  parts: Vec<ArrayPart>,
}

impl Array {
  /// Returns a new array with elements.
  #[must_use]
  pub fn new(env: Env, elems: Vec<Expr>) -> Self {
    Self { parts: ArrayPart::new(env, elems).into_iter().collect() }
  }

  /// Iterates over the elements in order.
  pub fn iter(&self) -> impl Iterator<Item = (&Env, Expr)> {
    self.parts.iter().flat_map(|part| part.elems.iter().map(|&elem| (&part.env, elem)))
  }

  /// Gets the nth element.
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

  /// Appends another array to this.
  pub fn append(&mut self, other: &mut Self) {
    self.parts.append(&mut other.parts);
  }

  /// Returns the length of this.
  #[must_use]
  pub fn len(&self) -> usize {
    self.parts.iter().map(|x| x.elems.len()).sum()
  }

  /// Returns whether this is empty.
  #[must_use]
  pub fn is_empty(&self) -> bool {
    let ret = self.parts.is_empty();
    always!(ret == (self.len() == 0));
    ret
  }
}

#[derive(Debug, Clone)]
struct ArrayPart {
  env: Env,
  /// INVARIANT: non-empty.
  elems: Vec<Expr>,
}

impl ArrayPart {
  fn new(env: Env, elems: Vec<Expr>) -> Option<Self> {
    if elems.is_empty() {
      None
    } else {
      Some(Self { env, elems })
    }
  }
}

/// A function.
#[derive(Debug, Clone)]
pub enum Fn {
  /// A regular user-written function.
  Regular(RegularFn),
  /// A standard library function.
  Std(StdFn),
}

/// A regular user-written function.
#[derive(Debug, Clone)]
pub struct RegularFn {
  /// The env the default params get evaluated under.
  pub env: Env,
  /// The params, with optional defaults.
  ///
  /// We'd like to get good performance for lookup by both index for positional arguments and name
  /// for keyword arguments, but to do that we'd need to something like double the memory and
  /// store both a vec and a map. which we could do but we choose to not right now.
  pub params: Vec<(Id, Option<Expr>)>,
  /// The function body.
  pub body: Expr,
}
