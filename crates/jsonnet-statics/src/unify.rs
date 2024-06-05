//! Unification.

// TODO remove
#![allow(unused)]

use crate::ty;
use always::always;
use drop_bomb::DebugDropBomb;
use jsonnet_expr::{Prim, Str};
use rustc_hash::FxHashMap;

enum Error {
  Incompatible,
  MissingField,
  NotEnoughParams,
  MismatchedParamNames,
  WantOptionalParamGotRequired,
  ExtraRequiredParam,
  AllAlternativesIncompatible,
  OccursCheck,
}

struct St {
  errors: Vec<Error>,
  subst: FxHashMap<ty::Meta, MetaSubst>,
}

impl St {
  fn err(&mut self, e: Error) {
    self.errors.push(e);
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

enum MetaSubst {
  Ty(ty::Ty),
}

/// this checks and unifies `want` is compatible with `got`, but allows `got` to be more specific
/// than `want`. aka, got should be a subtype of want, i suppose.
fn get(st: &mut St, store: &ty::Store, want: ty::Ty, got: ty::Ty) {
  match (store.data(want), store.data(got)) {
    (ty::Data::Any, _)
    | (_, ty::Data::Any)
    | (ty::Data::Bool, ty::Data::Bool | ty::Data::Prim(Prim::Bool(_)))
    | (ty::Data::String, ty::Data::String | ty::Data::Prim(Prim::String(_)))
    | (ty::Data::Number, ty::Data::Number | ty::Data::Prim(Prim::Number(_))) => {}
    (ty::Data::Prim(want), ty::Data::Prim(got)) => {
      if want != got {
        st.err(Error::Incompatible);
      }
    }
    (ty::Data::Array(want), ty::Data::Array(got)) => get(st, store, *want, *got),
    (ty::Data::Object(want), ty::Data::Object(got)) => {
      for (name, want) in want {
        let Some(got) = got.get(name) else {
          st.err(Error::MissingField);
          continue;
        };
        get(st, store, *want, *got);
      }
      // ignore the fields that ARE in `got` but are NOT in `want`.
    }
    (ty::Data::Fn(want), ty::Data::Fn(got)) => {
      if want.params.len() > got.params.len() {
        st.err(Error::NotEnoughParams);
      }
      for (want, got) in want.params.iter().zip(got.params.iter()) {
        if want.id != got.id {
          st.err(Error::MismatchedParamNames);
        }
        // if we wanted a required argument, we can get either a required or optional argument.
        if !want.required && got.required {
          st.err(Error::WantOptionalParamGotRequired);
        }
        // ah yes, the famous contra-variance.
        get(st, store, got.ty, want.ty);
      }
      for got in got.params.iter().skip(want.params.len()) {
        if got.required {
          st.err(Error::ExtraRequiredParam);
        }
      }
      get(st, store, want.ret, got.ret);
    }
    (ty::Data::Meta(m), _) => get_meta(st, store, *m, got),
    (_, ty::Data::Meta(m)) => get_meta(st, store, *m, want),
    // need to put this first
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
      st.err(Error::AllAlternativesIncompatible);
    }
    _ => st.err(Error::Incompatible),
  }
}

fn get_meta(st: &mut St, store: &ty::Store, meta: ty::Meta, want: ty::Ty) {
  if let ty::Data::Meta(m) = store.data(want) {
    if meta == *m {
      return;
    }
  }
  if occurs_check(store, meta, want) {
    st.err(Error::OccursCheck);
    return;
  }
  always!(st.subst.insert(meta, MetaSubst::Ty(want)).is_none());
}

fn occurs_check(store: &ty::Store, meta: ty::Meta, ty: ty::Ty) -> bool {
  match store.data(ty) {
    ty::Data::Any | ty::Data::Bool | ty::Data::String | ty::Data::Number | ty::Data::Prim(_) => {
      false
    }
    ty::Data::Array(ty) => occurs_check(store, meta, *ty),
    ty::Data::Object(fields) => fields.values().any(|&ty| occurs_check(store, meta, ty)),
    ty::Data::Fn(func) => std::iter::once(func.ret)
      .chain(func.params.iter().map(|param| param.ty))
      .any(|ty| occurs_check(store, meta, ty)),
    ty::Data::Meta(m) => meta == *m,
    ty::Data::Union(tys) => tys.iter().any(|&ty| occurs_check(store, meta, ty)),
  }
}
