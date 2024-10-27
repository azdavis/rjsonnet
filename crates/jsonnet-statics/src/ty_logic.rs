//! Logical operations with types.

use crate::st;
use always::always;
use jsonnet_ty as ty;
use std::collections::BTreeSet;

/// returns the type that is BOTH x AND Y. when such a type doesn't exist (like when e.g. x is
/// string and y is number), return the empty union type (aka never, aka void).
///
/// a bit similar to unification.
pub(crate) fn and(st: &mut st::St<'_>, x: ty::Ty, y: ty::Ty) -> ty::Ty {
  // speed up a simple case
  if x == y {
    return x;
  }
  match (st.data(x), st.data(y)) {
    (ty::Data::Prim(ty::Prim::Any), _) => y,
    (_, ty::Data::Prim(ty::Prim::Any)) => x,
    (ty::Data::Prim(x), ty::Data::Prim(y)) => {
      always!(x != y, "should have returned already if x == y");
      ty::Ty::NEVER
    }
    (ty::Data::Array(x), ty::Data::Array(y)) => {
      let elem = and(st, *x, *y);
      // NOTE: see discussion of object fields for why we do not return never even if the elem is of
      // type never.
      st.get_ty(ty::Data::Array(elem))
    }
    (ty::Data::Object(x), ty::Data::Object(y)) => {
      let mut known = x.known.clone();
      let y_known = y.known.clone();
      let has_unknown = x.has_unknown && y.has_unknown;
      for (name, x) in &mut known {
        let Some(&y) = y_known.get(name) else { continue };
        *x = and(st, *x, y);
        // NOTE: we do NOT return never even if a field has type never. this is because the language
        // is lazy - we can have a lazy field of type never and not witness the never-ness if we
        // don't access that field. it's a little wonky.
      }
      st.get_ty(ty::Data::Object(ty::Object { known, has_unknown }))
    }
    (ty::Data::Fn(_), ty::Data::Fn(_)) => {
      // we could almost certainly be more permissive here, but this case is probably rare to run
      // into anyway.
      ty::Ty::NEVER
    }
    (ty::Data::Union(xs), _) => union_and(st, xs.clone(), y),
    (_, ty::Data::Union(ys)) => union_and(st, ys.clone(), x),
    _ => ty::Ty::NEVER,
  }
}

/// and distributes across or:
///
/// (a || b) && c == (a && c) || (b && c)
fn union_and(st: &mut st::St<'_>, xs: BTreeSet<ty::Ty>, y: ty::Ty) -> ty::Ty {
  let u = ty::Data::Union(xs.into_iter().map(|x| and(st, x, y)).collect());
  st.get_ty(u)
}

/// returns the type that is x minus anything in y. when such a type doesn't exist (like when x ==
/// y), return the empty union type (aka never, aka void).
///
/// it's like x && !y, where !y is like the union type of everything except y.
///
/// note that we do NOT handle the case where x is any by returning a big union type of everything
/// except y. this is "okay" because having any already makes the type system unsound.
#[allow(dead_code)]
pub(crate) fn minus(st: &mut st::St<'_>, x: ty::Ty, y: ty::Ty) -> ty::Ty {
  // speed up a simple case
  if x == y || x == ty::Ty::NEVER {
    return ty::Ty::NEVER;
  }
  match (st.data(x), st.data(y)) {
    (ty::Data::Prim(ty::Prim::Any), _) => ty::Ty::ANY,
    (_, ty::Data::Prim(ty::Prim::Any)) => ty::Ty::NEVER,
    (ty::Data::Prim(xp), ty::Data::Prim(yp)) => {
      always!(xp != yp, "should have returned already if x == y");
      x
    }
    (ty::Data::Array(x), ty::Data::Array(y)) => {
      let elem = minus(st, *x, *y);
      st.get_ty(ty::Data::Array(elem))
    }
    (ty::Data::Object(x), ty::Data::Object(y)) => {
      let mut known = x.known.clone();
      let y_known = y.known.clone();
      let has_unknown = x.has_unknown;
      for (name, x) in &mut known {
        let Some(&y) = y_known.get(name) else { continue };
        *x = minus(st, *x, y);
      }
      st.get_ty(ty::Data::Object(ty::Object { known, has_unknown }))
    }
    (ty::Data::Fn(_), ty::Data::Fn(_)) => {
      // this might not be totally right, but this case should be rare anyway.
      x
    }
    (ty::Data::Union(xs), _) => {
      // (a || b) - y = (a - y) || (b - y)
      let xs = xs.clone();
      let u = ty::Data::Union(xs.into_iter().map(|x| minus(st, x, y)).collect());
      st.get_ty(u)
    }
    (_, ty::Data::Union(ys)) => {
      // x - (a || b) = x - a - b
      let mut ret = x;
      let ys = ys.clone();
      for y in ys {
        ret = minus(st, ret, y);
      }
      ret
    }
    _ => x,
  }
}
