//! Unification.

// TODO remove
#![allow(unused)]

use crate::ty;
use drop_bomb::DebugDropBomb;
use jsonnet_expr::Prim;

enum Error {
  Incompatible,
  MissingField,
  NotEnoughParams,
  MismatchedParamNames,
  WantOptionalParamGotRequired,
  ExtraRequiredParam,
  AllAlternativesIncompatible,
}

struct St {
  errors: Vec<Error>,
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
    (ty::Data::Meta(m), ty) | (ty, ty::Data::Meta(m)) => {
      if let ty::Data::Meta(m2) = ty {
        if m == m2 {
          return;
        }
      }
      todo!("unify m with ty, setting a subst somewhere")
    }
    // need to put this first
    (_, ty::Data::Or(got)) => {
      // want must be ALL of the things got may be.
      for &got in got {
        get(st, store, want, got);
      }
    }
    (ty::Data::Or(want), _) => {
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
