//! Logical operations with types.

use crate::{Array, Data, Fn, MutStore, Object, Param, Prim, RegularFn, Ty, Union};
use always::always;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

/// Returns the type that is BOTH `x` AND `y`.
///
/// When such a type doesn't exist, e.g. when e.g. x is `string` and y is `number`, returns `never`.
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
    (&Data::Array(x), &Data::Array(y)) => {
      let elem = and(tys, x.elem, y.elem);
      // NOTE: see discussion of object fields for why we do not return never even if the elem is of
      // type never.
      tys.get(Data::Array(Array { elem, is_set: x.is_set && y.is_set }))
    }
    (Data::Object(x), Data::Object(y)) => {
      if definitely_lacks(x, y) || definitely_lacks(y, x) {
        // NOTE: we DO return never if the types cannot possibly intersect. this happens when one
        // object with no unknown fields lacks a known field on the other object.
        return Ty::NEVER;
      }
      let x = x.clone();
      let y = y.clone();
      let keys: BTreeSet<_> = x.known.keys().chain(y.known.keys()).collect();
      let mut known = BTreeMap::<jsonnet_expr::Str, Ty>::new();
      for key in keys {
        let &x = x.known.get(key).unwrap_or(&Ty::ANY);
        let &y = y.known.get(key).unwrap_or(&Ty::ANY);
        let ty = and(tys, x, y);
        always!(known.insert(key.clone(), ty).is_none());
        // NOTE: we do NOT return never even if a field has type never. this is because the language
        // is lazy - we can have a lazy field of type never and not witness the never-ness if we
        // don't access that field. it's a little wonky.
      }
      tys.get(Data::Object(Object { known, has_unknown: x.has_unknown && y.has_unknown }))
    }
    (Data::Fn(f), Data::Fn(g)) => {
      let ((fps, fr), (gps, gr)) = match (f.parts(), g.parts()) {
        (None, None) => {
          always!(matches!(f, Fn::Unknown));
          always!(matches!(g, Fn::Unknown));
          return x;
        }
        (Some(_), None) => {
          always!(matches!(g, Fn::Unknown));
          return x;
        }
        (None, Some(_)) => {
          always!(matches!(f, Fn::Unknown));
          return y;
        }
        (Some(x), Some(y)) => (x, y),
      };
      // these are necessary to give up the borrow on f and g
      #[expect(clippy::unnecessary_to_owned)]
      let mut fps = fps.to_owned().into_iter();
      #[expect(clippy::unnecessary_to_owned)]
      let mut gps = gps.to_owned().into_iter();
      let mut params = Vec::<Param>::new();
      loop {
        let (fp, gp) = match (fps.next(), gps.next()) {
          (None, None) => break,
          (Some(param), None) | (None, Some(param)) => {
            if param.required {
              return Ty::NEVER;
            }
            break;
          }
          (Some(fp), Some(gp)) => (fp, gp),
        };
        let id = if fp.id.is_unutterable() {
          fp.id
        } else if gp.id.is_unutterable() {
          gp.id
        } else if fp.id == gp.id {
          fp.id
        } else {
          return Ty::NEVER;
        };
        let ty = and(tys, fp.ty, gp.ty);
        let required = fp.required || gp.required;
        params.push(Param { id, ty, required });
      }
      let func = Fn::Regular(RegularFn { params, ret: and(tys, fr, gr) });
      tys.get(Data::Fn(func))
    }
    (Data::Union(xs), _) => union_and(tys, xs.clone(), y),
    (_, Data::Union(ys)) => union_and(tys, ys.clone(), x),
    _ => Ty::NEVER,
  }
}

/// returns whether `x` is known to definitely lack a field that is in `y`.
fn definitely_lacks(x: &Object, y: &Object) -> bool {
  !x.has_unknown && y.known.keys().any(|k| !x.known.contains_key(k))
}

/// and distributes across or:
///
/// (a || b) && c == (a && c) || (b && c)
fn union_and(tys: &mut MutStore<'_>, xs: Union, y: Ty) -> Ty {
  let iter = xs.into_iter().map(|x| and(tys, x, y));
  let u = Data::Union(iter.collect());
  tys.get(u)
}

/// Returns the type that is `ty` and has length `n`.
///
/// When such a type doesn't exist, e.g. when `ty` is `number`, returns `never`.
pub fn with_len(tys: &mut MutStore<'_>, ty: Ty, n: usize) -> Ty {
  match tys.data(ty) {
    Data::Prim(prim) => match prim {
      Prim::Any => Ty::ANY,
      // no special type for strings with known length
      Prim::String => Ty::STRING,
      // these do not have length
      Prim::True | Prim::False | Prim::Null | Prim::Number => Ty::NEVER,
    },
    // no tuple types
    Data::Array(_) => ty,
    Data::Object(obj) => match (obj.known.len().cmp(&n), obj.has_unknown) {
      // 1. there may be unknown fields
      // 2. already have no unknown fields
      (Ordering::Less, true) | (Ordering::Equal, false) => ty,
      // known to have exactly the known fields and no more
      (Ordering::Equal, true) => {
        let obj = Object { known: obj.known.clone(), has_unknown: false };
        tys.get(Data::Object(obj))
      }
      // 1. can't have fewer known fields if no unknown fields
      // 2. known to have more fields than n
      (Ordering::Less, false) | (Ordering::Greater, _) => Ty::NEVER,
    },
    Data::Fn(func) => {
      if let Some((params, _)) = func.parts() {
        let required = params.iter().filter(|x| x.required).count();
        if required == n {
          ty
        } else {
          Ty::NEVER
        }
      } else if n > Param::UNUTTERABLE.len() {
        // we don't have infinite of these
        return ty;
      } else {
        let params = Param::UNUTTERABLE.iter().copied().take(n);
        let f = RegularFn { params: params.collect(), ret: Ty::ANY };
        tys.get(Data::Fn(Fn::Regular(f)))
      }
    }
    Data::Union(xs) => {
      let xs = xs.clone();
      let iter = xs.into_iter().map(|ty| with_len(tys, ty, n));
      let u = Data::Union(iter.collect());
      tys.get(u)
    }
  }
}

/// Returns the type that is x minus anything in y.
///
/// When such a type doesn't exist, e.g. when `x == y`, returns `never`.
///
/// It's kind of like `x && !y`, where `!y` is like the union of everything EXCEPT `y`.
pub fn minus(tys: &mut MutStore<'_>, x: Ty, y: Ty) -> Ty {
  // speed up simple cases (correctness is maintained if these are removed)
  if x == y || x == Ty::NEVER {
    return Ty::NEVER;
  }
  if y == Ty::NEVER {
    return x;
  }
  match (tys.data(x), tys.data(y)) {
    (Data::Prim(Prim::Any), _) => minus(tys, Ty::TOP, y),
    (_, Data::Prim(Prim::Any)) => Ty::NEVER,
    (Data::Prim(xp), Data::Prim(yp)) => {
      always!(xp != yp, "should have returned already if x == y");
      x
    }
    (Data::Array(x), Data::Array(y)) => {
      let elem = minus(tys, x.elem, y.elem);
      // NOTE: there's probably something more complicated we could do to determine whether the
      // result type should be a set, but it's conservatively correct to just never have it be.
      tys.get(Data::Array(Array::new(elem)))
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
