//! Jsonnet values.

use jsonnet_expr::{Expr, Id, Prim, Str, Visibility};
use rustc_hash::{FxHashMap, FxHashSet};

#[derive(Debug, Default, Clone)]
pub struct Env {
  store: FxHashMap<Id, (Env, Expr)>,
  this: Option<Box<Object>>,
}

impl Env {
  pub(crate) fn insert(&mut self, id: Id, env: Env, expr: Expr) {
    self.store.insert(id, (env, expr));
  }

  #[must_use]
  pub(crate) fn get(&self, id: Id) -> Get<'_> {
    if id == Id::SELF {
      return Get::Self_;
    }
    if id == Id::SUPER {
      return Get::Super;
    }
    if id == Id::STD_UNUTTERABLE {
      return Get::Std;
    }
    if let Some(&(ref env, expr)) = self.store.get(&id) {
      Get::Expr(env, expr)
    } else {
      assert_eq!(id, Id::STD, "get failed: {id:?}");
      Get::Std
    }
  }

  /// # Panics
  ///
  /// If this was not in scope.
  #[must_use]
  pub(crate) fn this(&self) -> &Object {
    self.this.as_deref().expect("`self` not in scope")
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

  #[must_use]
  pub(crate) fn new(
    env: Env,
    asserts: Vec<Expr>,
    fields: FxHashMap<Str, (Visibility, Expr)>,
  ) -> Self {
    let kind = ObjectKind::Regular(RegularObjectKind { env, asserts, fields });
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

  fn set_this(&self, env: &Env) -> Env {
    let mut env = env.clone();
    let mut this = self.clone();
    this.is_super = false;
    env.this = Some(Box::new(this));
    env
  }

  pub(crate) fn asserts(&self) -> impl Iterator<Item = (Env, Expr)> + '_ {
    let iter = self
      .ancestry()
      .filter_map(|this| match &this.kind {
        ObjectKind::Regular(this) => Some(this),
        ObjectKind::Std => None,
      })
      .flat_map(|this| this.asserts.iter().map(move |&expr| (&this.env, expr)));
    iter.map(|(env, expr)| (self.set_this(env), expr))
  }

  /// TODO this should be a generator
  pub(crate) fn fields(&self) -> Vec<(Str, Visibility, Field)> {
    let mut ret = Vec::<(Str, Visibility, Field)>::new();
    let mut seen = FxHashSet::<&Str>::default();
    for this in self.ancestry().skip(self.is_super.into()) {
      match &this.kind {
        ObjectKind::Regular(this) => {
          for (name, &(vis, expr)) in &this.fields {
            if !seen.insert(name) {
              continue;
            }
            ret.push((name.clone(), vis, Field::Expr(self.set_this(&this.env), expr)));
          }
        }
        ObjectKind::Std => {
          for (name, field) in &StdField::ALL {
            if !seen.insert(name) {
              continue;
            }
            ret.push((name.clone(), Visibility::Hidden, Field::Std(field.clone())));
          }
        }
      }
    }
    ret
  }

  #[must_use]
  pub(crate) fn get_field(&self, name: &Str) -> Option<(Visibility, Field)> {
    self.ancestry().skip(self.is_super.into()).find_map(|this| match &this.kind {
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
  asserts: Vec<Expr>,
  fields: FxHashMap<Str, (Visibility, Expr)>,
}

#[derive(Debug, Clone)]
pub enum Field {
  Std(StdField),
  Expr(Env, Expr),
}

#[derive(Debug, Clone)]
pub enum StdField {
  ThisFile,
  Fn(StdFn),
}

impl StdField {
  const ALL: [(Str, StdField); 3] = [
    (Str::THIS_FILE, StdField::ThisFile),
    (Str::CMP, StdField::Fn(StdFn::Cmp)),
    (Str::EQUALS, StdField::Fn(StdFn::Equals)),
  ];
}

impl TryFrom<&Str> for StdField {
  type Error = ();

  fn try_from(s: &Str) -> Result<Self, Self::Error> {
    if *s == Str::THIS_FILE {
      return Ok(Self::ThisFile);
    }
    if *s == Str::CMP {
      return Ok(Self::Fn(StdFn::Cmp));
    }
    if *s == Str::EQUALS {
      return Ok(Self::Fn(StdFn::Equals));
    }
    Err(())
  }
}

#[derive(Debug, Clone)]
pub enum StdFn {
  Cmp,
  Equals,
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
