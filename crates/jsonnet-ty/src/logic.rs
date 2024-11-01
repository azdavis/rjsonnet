//! Logical operations with types.

use crate::{Data, MutStore, Object, Prim, Ty, Union};
use always::always;

/// Returns the type that is BOTH x AND y.
///
/// When such a type doesn't exist (like when e.g. x is string and y is number), return the empty
/// union type (aka never, aka void).
///
/// A bit similar to unification.
pub fn and(tys: &mut MutStore<'_>, x: Ty, y: Ty) -> Ty {
  // speed up simple cases (correctness is maintained if these are removed)
  if x == y || y == Ty::ANY {
    return x;
  }
  match (tys.data(x), tys.data(y)) {
    (Data::Prim(Prim::Any), _) => y,
    (_, Data::Prim(Prim::Any)) => x,
    (Data::Prim(x), Data::Prim(y)) => {
      always!(x != y, "should have returned already if x == y");
      Ty::NEVER
    }
    (Data::Array(x), Data::Array(y)) => {
      let elem = and(tys, *x, *y);
      // NOTE: see discussion of object fields for why we do not return never even if the elem is of
      // type never.
      tys.get(Data::Array(elem))
    }
    (Data::Object(x), Data::Object(y)) => {
      let mut known = x.known.clone();
      let y_known = y.known.clone();
      let has_unknown = x.has_unknown && y.has_unknown;
      for (name, x) in &mut known {
        let Some(&y) = y_known.get(name) else { continue };
        *x = and(tys, *x, y);
        // NOTE: we do NOT return never even if a field has type never. this is because the language
        // is lazy - we can have a lazy field of type never and not witness the never-ness if we
        // don't access that field. it's a little wonky.
      }
      tys.get(Data::Object(Object { known, has_unknown }))
    }
    (Data::Fn(_), Data::Fn(_)) => {
      // we could almost certainly be more permissive here, but this case is probably rare to run
      // into anyway.
      Ty::NEVER
    }
    (Data::Union(xs), _) => union_and(tys, xs.clone(), y),
    (_, Data::Union(ys)) => union_and(tys, ys.clone(), x),
    _ => Ty::NEVER,
  }
}

/// and distributes across or:
///
/// (a || b) && c == (a && c) || (b && c)
fn union_and(tys: &mut MutStore<'_>, xs: Union, y: Ty) -> Ty {
  let u = Data::Union(xs.into_iter().map(|x| and(tys, x, y)).collect());
  tys.get(u)
}

/// Returns the type that is x minus anything in y.
///
/// When such a type doesn't exist (like when x == y), return the empty union type (aka never, aka
/// void).
///
/// It's like x && !y, where !y is like the union type of everything except y.
///
/// Note that we do NOT handle the case where x is any by returning a big union type of everything
/// except y. This is "okay" because having any already makes the type system unsound.
pub fn minus(tys: &mut MutStore<'_>, x: Ty, y: Ty) -> Ty {
  // speed up simple cases (correctness is maintained if these are removed)
  if x == y || x == Ty::NEVER {
    return Ty::NEVER;
  }
  if y == Ty::NEVER {
    return x;
  }
  match (tys.data(x), tys.data(y)) {
    (Data::Prim(Prim::Any), _) => Ty::ANY,
    (_, Data::Prim(Prim::Any)) => Ty::NEVER,
    (Data::Prim(xp), Data::Prim(yp)) => {
      always!(xp != yp, "should have returned already if x == y");
      x
    }
    (Data::Array(x), Data::Array(y)) => {
      let elem = minus(tys, *x, *y);
      tys.get(Data::Array(elem))
    }
    (Data::Object(x), Data::Object(y)) => {
      let mut known = x.known.clone();
      let y_known = y.known.clone();
      let has_unknown = x.has_unknown;
      for (name, x) in &mut known {
        let Some(&y) = y_known.get(name) else { continue };
        *x = minus(tys, *x, y);
      }
      tys.get(Data::Object(Object { known, has_unknown }))
    }
    (Data::Fn(_), Data::Fn(_)) => {
      // this might not be totally right, but this case should be rare anyway.
      x
    }
    (Data::Union(xs), _) => {
      // (a || b) - y = (a - y) || (b - y)
      let xs = xs.clone();
      let u = Data::Union(xs.into_iter().map(|x| minus(tys, x, y)).collect());
      tys.get(u)
    }
    (_, Data::Union(ys)) => {
      // x - (a || b) = x - a - b
      let mut ret = x;
      let ys = ys.clone();
      for y in ys {
        ret = minus(tys, ret, y);
      }
      ret
    }
    _ => x,
  }
}
