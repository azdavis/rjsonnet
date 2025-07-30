//! Jsonnet values.

use always::always;
use cycle::Cycle;
use jsonnet_expr::{Expr, ExprArena, ExprData, Id, Prim, StdField, StdFn, Str, StrArena, Vis};
use rustc_hash::{FxHashMap, FxHashSet};

/// An environment, which stores a mapping of identifiers to unevaluated expressions.
#[derive(Debug, Clone)]
pub struct Env {
  store: Vec<EnvElem>,
  cycle_detector: cycle::Detector<paths::PathId>,
}

impl Env {
  /// Returns the path that this env came from.
  #[must_use]
  pub fn path(&self) -> paths::PathId {
    *self.cycle_detector.cur()
  }

  /// Adds the binds to the env.
  pub fn add_binds(&mut self, binds: Vec<(Id, Expr)>, self_refer: SelfRefer) {
    if binds.is_empty() {
      return;
    }
    self.store.push(EnvElem::Binds(binds, self_refer));
  }

  /// Returns an empty env.
  #[must_use]
  pub fn empty(path: paths::PathId) -> Self {
    Self { store: Vec::new(), cycle_detector: cycle::Detector::new(path) }
  }

  /// Append `other` after `self`, leaving `other` empty.
  pub fn append(&mut self, other: &mut Self) {
    self.store.append(&mut other.store);
  }

  /// Returns an empty env, but check to see if this would cause a cycle.
  ///
  /// # Errors
  ///
  /// If there would be a cycle.
  pub fn empty_with_paths(&self, path: paths::PathId) -> Result<Self, Cycle<paths::PathId>> {
    Ok(Self { store: Vec::new(), cycle_detector: self.cycle_detector.clone().try_push(path)? })
  }

  /// Insert an id-expr mapping.
  pub fn insert(&mut self, subst: Subst) {
    self.store.push(EnvElem::Single(subst));
  }

  /// Causes `self` and `super` to reference the `$outerself` and `$outersuper` variables. Use with
  /// caution.
  pub fn use_outer_self_super(&mut self) {
    self.store.push(EnvElem::Outer);
  }

  /// Removes an id from this env. Use with caution.
  pub fn remove(&mut self, id: Id) {
    let iter = std::mem::take(&mut self.store).into_iter().filter_map(|elem| match elem {
      EnvElem::Binds(vec, self_refer) => {
        let iter = vec.into_iter().filter(|&(x, _)| x != id);
        Some(EnvElem::Binds(iter.collect(), self_refer))
      }
      EnvElem::Single(subst) => {
        if subst.id == id {
          None
        } else {
          Some(EnvElem::Single(subst))
        }
      }
      EnvElem::This(object) => Some(EnvElem::This(object)),
      EnvElem::Outer => Some(EnvElem::Outer),
    });
    self.store = iter.collect();
  }

  /// Get an identifier. Returns `None` if it is not in scope.
  #[must_use]
  pub fn get(&self, mut id: Id) -> Option<ValOrExpr> {
    if id == Id::self_ {
      match self.this()? {
        This::Object(obj) => return Some(ValOrExpr::Val(obj.clone().into())),
        This::Outer => id = Id::outerself_unutterable,
      }
    }
    if id == Id::super_ {
      match self.this()? {
        This::Object(obj) => return Some(ValOrExpr::Val(obj.parent().into())),
        This::Outer => id = Id::outersuper_unutterable,
      }
    }
    if id == Id::std_unutterable {
      return Some(ValOrExpr::Val(Object::std_lib().into()));
    }
    for idx in (0..self.store.len()).rev() {
      match &self.store[idx] {
        EnvElem::This(_) | EnvElem::Outer => {}
        EnvElem::Binds(binds, self_refer) => {
          for &(other, expr) in binds {
            if other == id {
              let extra = match self_refer {
                SelfRefer::Yes => 1,
                SelfRefer::No => 0,
              };
              let env = Self {
                store: self.store.iter().take(idx + extra).cloned().collect(),
                cycle_detector: self.cycle_detector.clone(),
              };
              return Some(ValOrExpr::Expr(env, expr));
            }
          }
        }
        EnvElem::Single(subst) => {
          if subst.id == id {
            return Some(subst.val.clone());
          }
        }
      }
    }
    (id == Id::std).then(|| ValOrExpr::Val(Object::std_lib().into()))
  }

  /// Returns what `self` refers to in this env.
  #[must_use]
  fn this(&self) -> Option<This<'_>> {
    self.store.iter().rev().find_map(|elem| match elem {
      EnvElem::This(obj) => Some(This::Object(obj)),
      EnvElem::Outer => Some(This::Outer),
      _ => None,
    })
  }
}

#[derive(Debug, Clone)]
enum EnvElem {
  Binds(Vec<(Id, Expr)>, SelfRefer),
  Single(Subst),
  This(Box<Object>),
  Outer,
}

enum This<'a> {
  Object(&'a Object),
  Outer,
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

impl From<Array> for Val {
  fn from(xs: Array) -> Self {
    Val::Array(xs)
  }
}

impl From<Object> for Val {
  fn from(obj: Object) -> Self {
    Val::Object(obj)
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
    env.store.push(EnvElem::This(Box::new(this)));
    env
  }

  /// Returns the asserts in this.
  pub fn asserts(&self) -> impl Iterator<Item = (Env, Expr)> {
    let iter = self
      .ancestry()
      .filter_map(|this| match &this.kind {
        ObjectKind::Regular(this) => Some(this),
        ObjectKind::Std => None,
      })
      .flat_map(|this| this.asserts.iter().map(move |&expr| (this, expr)));
    iter.map(|(this, expr)| (self.set_this(&this.env), expr))
  }

  /// Returns the visible fields.
  ///
  /// The fields will be sorted in some stable order, i.e. calls to this method on the same object
  /// will return the same result each time. However, that order is likely not the lexicographic
  /// ordering of the string keys.
  ///
  /// TODO should this be a generator?
  #[must_use]
  pub fn visible_fields(&self) -> Vec<(Str, Env, Expr)> {
    let mut ret = self.visible_fields_unstable();
    // ok to sort unstable because there will not be duplicate keys
    ret.sort_unstable_by_key(|&(name, _, _)| name);
    ret
  }

  /// Returns the visible fields, sorted in order of the string keys.
  #[must_use]
  pub fn sorted_visible_fields(&self, ar: &StrArena) -> Vec<(Str, Env, Expr)> {
    let mut ret = self.visible_fields_unstable();
    // ok to sort unstable because there will not be duplicate keys
    ret.sort_unstable_by_key(|&(name, _, _)| ar.get(name));
    ret
  }

  /// Returns all the fields sorted in string order.
  ///
  /// Ignore the visibility on the fields, it's meaningless.
  #[must_use]
  pub fn all_sorted_fields(&self, ar: &StrArena) -> Vec<(Str, Field)> {
    let mut fs = self.fields();
    let hidden = std::mem::take(&mut fs.hidden);
    let iter = self
      .visible(fs)
      .map(|(str, env, expr)| (str, Field::Expr(Vis::Default, env, expr)))
      .chain(hidden);
    let mut ret: Vec<_> = iter.collect();
    // ok to sort unstable because there will not be duplicate keys
    ret.sort_unstable_by_key(|&(name, _)| ar.get(name));
    ret
  }

  fn fields(&self) -> Fields<'_> {
    let mut ret = Fields::default();
    for this in self.ancestry_considering_superness() {
      let this = match &this.kind {
        ObjectKind::Regular(x) => x,
        ObjectKind::Std => {
          for (name, field) in StdField::ALL {
            if !ret.visible.contains_key(&name) {
              ret.hidden.entry(name).or_insert(Field::Std(field));
            }
          }
          continue;
        }
      };
      for (&name, field) in &this.fields {
        if ret.visible.contains_key(&name) || ret.hidden.contains_key(&name) {
          continue;
        }
        match field.vis {
          Vis::Default => {
            ret.default.entry(name).or_insert((this, field));
          }
          Vis::Hidden => {
            ret.hidden.entry(name).or_insert(Field::Expr(
              Vis::Hidden,
              this.env.clone(),
              field.expr,
            ));
          }
          Vis::Visible => {
            ret.visible.insert(name, (this, field));
          }
        }
      }
    }
    ret
  }

  fn visible_fields_unstable(&self) -> Vec<(Str, Env, Expr)> {
    let mut fs = self.fields();
    for name in fs.hidden.keys() {
      fs.default.remove(name);
    }
    self.visible(fs).collect()
  }

  fn visible(&self, fs: Fields<'_>) -> impl Iterator<Item = (Str, Env, Expr)> {
    fs.visible.into_iter().chain(fs.default).map(|(name, (this, field))| {
      let mut env = self.set_this(&this.env);
      if let Some(subst) = &field.comp_subst {
        env.insert(subst.clone());
      }
      (name, env, field.expr)
    })
  }

  /// Gets a field off an object.
  #[must_use]
  pub fn get_field(&self, name: Str) -> Option<Field> {
    self.ancestry_considering_superness().find_map(|this| match &this.kind {
      ObjectKind::Std => {
        let field = StdField::try_from(name).ok()?;
        Some(Field::Std(field))
      }
      ObjectKind::Regular(this) => {
        let field = this.fields.get(&name)?;
        Some(Field::Expr(field.vis, self.set_this(&this.env), field.expr))
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

#[derive(Debug, Default)]
struct Fields<'a> {
  visible: FxHashMap<Str, (&'a RegularObjectKind, &'a ExprField)>,
  hidden: FxHashMap<Str, Field>,
  default: FxHashMap<Str, (&'a RegularObjectKind, &'a ExprField)>,
}

#[derive(Debug, Clone)]
enum ObjectKind {
  Std,
  Regular(RegularObjectKind),
}

#[derive(Debug, Clone)]
struct RegularObjectKind {
  id: uniq::Uniq,
  env: Env,
  asserts: Vec<Expr>,
  fields: ExprFields,
}

/// A maker of objects.
#[derive(Debug, Default)]
pub struct ObjectMk {
  id_mk: uniq::UniqMk,
  /// needed to avoid cyclically checking object asserts. we must check object asserts when getting
  /// a field from on an object, but we may get a field from an object in an assert in the object
  /// itself with `self`.
  checking_asserts: FxHashSet<uniq::Uniq>,
}

impl ObjectMk {
  /// Returns a new regular (non-std) object.
  #[must_use]
  pub fn mk(&mut self, env: Env, asserts: Vec<Expr>, fields: ExprFields) -> Object {
    let id = self.id_mk.mk();
    let kind = ObjectKind::Regular(RegularObjectKind { id, env, asserts, fields });
    Object { parent: None, kind, is_super: false }
  }

  /// Returns whether we should actually start checking the asserts on the object.
  ///
  /// That is, if this returns `false`, we are already checking the asserts, or there are no asserts
  /// to check.
  pub fn start_checking_asserts(&mut self, object: &Object) -> bool {
    match &object.kind {
      ObjectKind::Std => false,
      ObjectKind::Regular(obj) => self.checking_asserts.insert(obj.id),
    }
  }

  /// Finishes checking asserts on the object.
  pub fn finish_checking_asserts(&mut self, object: &Object) {
    let ret = match &object.kind {
      ObjectKind::Std => false,
      ObjectKind::Regular(obj) => self.checking_asserts.remove(&obj.id),
    };
    always!(ret, "should only finish checking asserts once started");
  }
}

/// Expr fields, in a deterministic order.
pub type ExprFields = std::collections::BTreeMap<Str, ExprField>;

/// An expr field.
#[derive(Debug, Clone)]
pub struct ExprField {
  /// The visibility.
  pub vis: Vis,
  /// The expression.
  pub expr: Expr,
  /// An extra subst for object comprehensions.
  pub comp_subst: Option<Subst>,
}

/// An subst in an env.
#[derive(Debug, Clone)]
pub struct Subst {
  /// The id to subst.
  pub id: Id,
  /// The val or expr to subst for the id.
  pub val: ValOrExpr,
}

/// An object field.
#[derive(Debug)]
pub enum Field {
  /// A standard library field. Always hidden.
  Std(StdField),
  /// A regular expression field.
  Expr(Vis, Env, Expr),
}

impl Field {
  /// Returns whether this is visible.
  #[must_use]
  pub fn is_visible(&self) -> bool {
    match self {
      Field::Std(_) => false,
      Field::Expr(vis, _, _) => vis.is_visible(),
    }
  }

  /// Returns this as an (env, expr) pair.
  pub fn into_expr(self, ar: &mut ExprArena, p: paths::PathId) -> (Env, Expr) {
    match self {
      Field::Std(field) => (Env::empty(p), Some(std_field_expr(ar, field))),
      Field::Expr(_, env, e) => (env, e),
    }
  }
}

fn std_field_expr(ar: &mut ExprArena, field: StdField) -> jsonnet_expr::ExprMust {
  let on = Some(ar.alloc(ExprData::Id(Id::std_unutterable)));
  let idx = Some(ar.alloc(ExprData::Prim(Prim::String(field.as_builtin_str()))));
  ar.alloc(ExprData::Subscript { on, idx })
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

  /// Returns the elements in order.
  ///
  /// TODO this should be a generator
  #[must_use]
  pub fn elems(&self) -> Vec<(&Env, Expr)> {
    let mut ret = Vec::<(&Env, Expr)>::new();
    for part in &self.parts {
      ret.extend(part.elems.iter().map(|&x| (&part.env, x)));
    }
    ret
  }

  /// Gets the nth element.
  #[must_use]
  pub fn get(&self, mut idx: usize) -> Option<(&Env, Expr)> {
    for part in &self.parts {
      match part.get(idx) {
        Some(x) => return Some((&part.env, x)),
        None => idx = idx.checked_sub(part.len())?,
      }
    }
    None
  }

  /// Appends another array to this.
  pub fn append(&mut self, other: &mut Self) {
    self.parts.append(&mut other.parts);
  }

  /// Pushes an element onto this.
  ///
  /// If a single `Env` is shared across many `Expr`s, it's preferable to create an `Array` via `new`
  /// and then `append` it as opposed to repeatedly calling `push` with the cloned `Env`.
  pub fn push(&mut self, env: Env, elem: Expr) {
    self.parts.push(ArrayPart { env, elems: vec![elem] });
  }

  /// Returns the length of this.
  #[must_use]
  pub fn len(&self) -> usize {
    self.parts.iter().map(ArrayPart::len).sum()
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
  /// INVARIANT: vec is non-empty.
  elems: Vec<Expr>,
}

impl ArrayPart {
  fn new(env: Env, elems: Vec<Expr>) -> Option<Self> {
    if elems.is_empty() { None } else { Some(Self { env, elems }) }
  }

  fn len(&self) -> usize {
    self.elems.len()
  }

  fn get(&self, idx: usize) -> Option<Expr> {
    self.elems.get(idx).copied()
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
  /// The env the default params and body get evaluated under.
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

/// Whether some bindings can refer to themselves.
#[derive(Debug, Clone, Copy)]
pub enum SelfRefer {
  /// They may.
  Yes,
  /// They may not.
  No,
}

/// Either a val or an expression.
#[derive(Debug, Clone)]
pub enum ValOrExpr {
  /// A value.
  Val(Val),
  /// An expression.
  Expr(Env, Expr),
}
