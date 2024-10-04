//! Unification of types.

use crate::error;
use drop_bomb::DebugDropBomb;
use jsonnet_ty as ty;

pub(crate) struct St<'a> {
  pub(crate) expr: jsonnet_expr::ExprMust,
  pub(crate) errors: &'a mut Vec<error::Error>,
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
pub(crate) fn get(st: &mut St<'_>, store: &ty::MutStore<'_>, want: ty::Ty, got: ty::Ty) {
  match (store.data(want), store.data(got)) {
    (ty::Data::Prim(ty::Prim::Any), _) | (_, ty::Data::Prim(ty::Prim::Any)) => {}
    (ty::Data::Prim(w), ty::Data::Prim(g)) => {
      if w != g {
        st.err(error::Kind::Incompatible(want, got));
      }
    }
    (ty::Data::Array(want), ty::Data::Array(got)) => get(st, store, *want, *got),
    (ty::Data::Object(want), ty::Data::Object(got)) => {
      for (name, w) in &want.known {
        let Some(g) = got.known.get(name) else {
          if !got.has_unknown {
            st.err(error::Kind::MissingField(name.clone()));
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
    (ty::Data::Fn(ty::Fn::Regular(want)), ty::Data::Fn(ty::Fn::Regular(got))) => {
      if want.params.len() > got.params.len() {
        st.err(error::Kind::NotEnoughParams(want.params.len(), got.params.len()));
      }
      for (want, got) in want.params.iter().zip(got.params.iter()) {
        if want.id != got.id {
          st.err(error::Kind::MismatchedParamNames(want.id, got.id));
        }
        // if we wanted a required argument, we can get either a required or optional argument.
        if !want.required && got.required {
          st.err(error::Kind::WantOptionalParamGotRequired(want.id));
        }
        // ah yes, the famous contra-variance.
        get(st, store, got.ty, want.ty);
      }
      for got in got.params.iter().skip(want.params.len()) {
        if got.required {
          st.err(error::Kind::ExtraRequiredParam(got.id));
        }
      }
      get(st, store, want.ret, got.ret);
    }
    (ty::Data::Fn(ty::Fn::Std(w)), ty::Data::Fn(ty::Fn::Std(g))) => {
      // NOTE this is overly strict
      if w != g {
        st.err(error::Kind::Incompatible(want, got));
      }
    }
    (ty::Data::Fn(ty::Fn::Hof(w)), ty::Data::Fn(g)) => {
      let got_required = match g {
        ty::Fn::Regular(g) => g.params.iter().filter(|x| x.required).count(),
        ty::Fn::Std(g) => ty::StdFnSig::get(*g).params.iter().filter(|x| x.required).count(),
        ty::Fn::Hof(g) => g.to_usize(),
      };
      if w.to_usize() != got_required {
        st.err(error::Kind::NotEnoughParams(w.to_usize(), got_required));
      }
    }
    (ty::Data::Fn(ty::Fn::Std(_)), ty::Data::Fn(ty::Fn::Regular(_)))
    | (ty::Data::Fn(ty::Fn::Regular(_)), ty::Data::Fn(ty::Fn::Std(_) | ty::Fn::Hof(_))) => {
      // TODO do more here
      log::warn!("unify various fn types");
    }
    // need to put this (got-union) before the next (want-union)
    (_, ty::Data::Union(got)) => {
      // want must be ALL of the things got may be.
      for &g in got {
        get(st, store, want, g);
      }
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
      st.err(error::Kind::Incompatible(want, got));
    }
    _ => st.err(error::Kind::Incompatible(want, got)),
  }
}
