//! Unification of types.

use crate::error;
use always::always;
use drop_bomb::DebugDropBomb;
use jsonnet_ty as ty;

#[derive(Debug, Default)]
pub(crate) struct St {
  errors: Vec<error::Unify>,
}

impl St {
  pub(crate) fn finish(self) -> Vec<error::Unify> {
    self.errors
  }

  fn err(&mut self, e: error::Unify) {
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
pub(crate) fn get(st: &mut St, store: &ty::MutStore<'_>, want: ty::Ty, got: ty::Ty) {
  // speed up simple cases (correctness is maintained if these are removed)
  if want == got || want == ty::Ty::ANY || got == ty::Ty::ANY {
    return;
  }
  match (store.data(want), store.data(got)) {
    (ty::Data::Prim(ty::Prim::Any), _) | (_, ty::Data::Prim(ty::Prim::Any)) => {}
    (ty::Data::Prim(w), ty::Data::Prim(g)) => {
      if always!(w != g, "should have returned already if want == got") {
        st.err(error::Unify::Incompatible(want, got));
      }
    }
    (&ty::Data::Array(ty::Array { elem: want }), &ty::Data::Array(ty::Array { elem: got })) => {
      get(st, store, want, got);
    }
    (ty::Data::Object(want), ty::Data::Object(got)) => {
      for (name, w) in &want.known {
        let Some(g) = got.known.get(name) else {
          if !got.has_unknown {
            st.err(error::Unify::NoSuchField(name.clone()));
          }
          continue;
        };
        get(st, store, *w, *g);
      }
      // we used to error when `want.has_unknown && !got.has_unknown`. the idea was that this may
      // arguably be an error, since it means that we are doing something like e.g. `"foo" in {}`,
      // i.e. we are asking if there is a certain field in an object whose fields we know
      // statically.
      //
      // but we do not emit an error here since it would probably be disruptive and unexpected to
      // users.
      //
      // further, it is sound to not emit an error here. since the only operation that we could do
      // when wanting unknown fields is to query for whatever fields we wanted, which MAY fail:
      // since we don't know what fields we have, the field may or may not exist.
      //
      // when we know we're getting something with NO unknown fields, the only thing that changes is
      // that we KNOW those unknown field gets will fail every time. which is fine.
      //
      // also, ignore the fields that ARE in `got` but are NOT in `want`. kind of like sub-typing.
    }
    (ty::Data::Fn(want), ty::Data::Fn(got)) => {
      let (want_params, want_ret) = want.parts();
      let (got_params, got_ret) = got.parts();
      if let (Some(want_params), Some(got_params)) = (want_params, got_params) {
        if want_params.len() > got_params.len() {
          st.err(error::Unify::NotEnoughParams(want_params.len(), got_params.len()));
        }
        for (want, got) in want_params.iter().zip(got_params.iter()) {
          if !want.id.is_builtin_unutterable() && want.id != got.id {
            st.err(error::Unify::MismatchedParamNames(want.id, got.id));
          }
          // if we wanted a required argument, we can get either a required or optional argument.
          if !want.required && got.required {
            st.err(error::Unify::WantOptionalParamGotRequired(want.id));
          }
          // ah yes, the famous contra-variance.
          get(st, store, got.ty, want.ty);
        }
        for got in got_params.iter().skip(want_params.len()) {
          if got.required {
            st.err(error::Unify::ExtraRequiredParam(got.id));
          }
        }
      }
      get(st, store, want_ret, got_ret);
    }
    (ty::Data::Union(tys), _) => {
      // got may be ANY of the things want may be.
      for &w in tys {
        let m = st.mark();
        get(st, store, w, got);
        if !st.discard_after(m) {
          // this unify was OK
          return;
        }
      }
      st.err(error::Unify::Incompatible(want, got));
    }
    (_, ty::Data::Union(got)) => {
      // want must be ALL of the things got may be.
      for &g in got {
        get(st, store, want, g);
      }
    }
    _ => st.err(error::Unify::Incompatible(want, got)),
  }
}
