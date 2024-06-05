//! Unification.

use crate::{error, ty};
use drop_bomb::DebugDropBomb;
use jsonnet_expr::Prim;

pub(crate) struct St<'a> {
  pub(crate) expr: jsonnet_expr::ExprMust,
  pub(crate) errors: &'a mut Vec<error::Error>,
  pub(crate) subst: &'a mut ty::Subst,
}

impl<'a> St<'a> {
  fn err(&mut self, kind: error::Kind) {
    self.errors.push(error::Error { expr: self.expr, kind });
  }

  fn mark(&self) -> Marker {
    Marker { bomb: DebugDropBomb::new("must discard marker"), len: self.errors.len() }
  }

  /// returns whether errors WERE discarded, i.e. there were errors done AFTER the marker.
  fn discard_after(&mut self, mut m: Marker) -> bool {
    m.bomb.defuse();
    let ret = self.errors.len() > m.len;
    self.errors.truncate(m.len);
    ret
  }
}

struct Marker {
  bomb: DebugDropBomb,
  len: usize,
}

/// this checks and unifies `want` is compatible with `got`, but allows `got` to be more specific
/// than `want`. aka, got should be a subtype of want, i suppose.
pub(crate) fn get(st: &mut St<'_>, store: &ty::Store, want: ty::Ty, got: ty::Ty) {
  match (store.data(st.subst, want), store.data(st.subst, got)) {
    (ty::Data::Any, _)
    | (_, ty::Data::Any)
    | (ty::Data::Bool, ty::Data::Bool | ty::Data::Prim(Prim::Bool(_)))
    | (ty::Data::String, ty::Data::String | ty::Data::Prim(Prim::String(_)))
    | (ty::Data::Number, ty::Data::Number | ty::Data::Prim(Prim::Number(_))) => {}
    (ty::Data::Prim(want), ty::Data::Prim(got)) => {
      if want != got {
        st.err(error::Kind::Incompatible);
      }
    }
    (ty::Data::Array(want), ty::Data::Array(got)) => get(st, store, *want, *got),
    (ty::Data::Object(want), ty::Data::Object(got)) => {
      for (name, want) in want {
        let Some(got) = got.get(name) else {
          st.err(error::Kind::MissingField);
          continue;
        };
        get(st, store, *want, *got);
      }
      // ignore the fields that ARE in `got` but are NOT in `want`.
    }
    (ty::Data::Fn(want), ty::Data::Fn(got)) => {
      if want.params.len() > got.params.len() {
        st.err(error::Kind::NotEnoughParams);
      }
      for (want, got) in want.params.iter().zip(got.params.iter()) {
        if want.id != got.id {
          st.err(error::Kind::MismatchedParamNames);
        }
        // if we wanted a required argument, we can get either a required or optional argument.
        if !want.required && got.required {
          st.err(error::Kind::WantOptionalParamGotRequired);
        }
        // ah yes, the famous contra-variance.
        get(st, store, got.ty, want.ty);
      }
      for got in got.params.iter().skip(want.params.len()) {
        if got.required {
          st.err(error::Kind::ExtraRequiredParam);
        }
      }
      get(st, store, want.ret, got.ret);
    }
    (ty::Data::Meta(m), _) => get_meta(st, store, *m, got),
    (_, ty::Data::Meta(m)) => get_meta(st, store, *m, want),
    // need to put this (got-union) before the next (want-union)
    (_, ty::Data::Union(got)) => {
      // want must be ALL of the things got may be.
      for &got in got {
        get(st, store, want, got);
      }
    }
    (ty::Data::Union(want), _) => {
      // got may be ANY of the things want may be.
      for &want in want {
        let m = st.mark();
        get(st, store, want, got);
        if !st.discard_after(m) {
          // this unify was OK
          return;
        }
      }
      st.err(error::Kind::AllAlternativesIncompatible);
    }
    _ => st.err(error::Kind::Incompatible),
  }
}

fn get_meta(st: &mut St<'_>, store: &ty::Store, meta: ty::Meta, want: ty::Ty) {
  if let ty::Data::Meta(m) = store.data(st.subst, want) {
    if meta == *m {
      return;
    }
  }
  if occurs_check(store, st.subst, meta, want) {
    st.err(error::Kind::OccursCheck);
    return;
  }
  st.subst.insert(meta, ty::MetaSubst::Ty(want));
}

fn occurs_check(store: &ty::Store, subst: &ty::Subst, m: ty::Meta, ty: ty::Ty) -> bool {
  match store.data(subst, ty) {
    ty::Data::Any | ty::Data::Bool | ty::Data::String | ty::Data::Number | ty::Data::Prim(_) => {
      false
    }
    ty::Data::Array(ty) => occurs_check(store, subst, m, *ty),
    ty::Data::Object(fields) => fields.values().any(|&ty| occurs_check(store, subst, m, ty)),
    ty::Data::Fn(func) => std::iter::once(func.ret)
      .chain(func.params.iter().map(|param| param.ty))
      .any(|ty| occurs_check(store, subst, m, ty)),
    ty::Data::Meta(m2) => m == *m2,
    ty::Data::Union(tys) => tys.iter().any(|&ty| occurs_check(store, subst, m, ty)),
  }
}
