//! Unification of types.

use crate::error;
use always::always;
use drop_bomb::DebugDropBomb;
use jsonnet_ty as ty;

#[derive(Debug)]
pub(crate) struct St<'a> {
  str_ar: &'a jsonnet_expr::StrArena,
  errors: Vec<error::Unify>,
}

impl<'a> St<'a> {
  pub(crate) fn new(str_ar: &'a jsonnet_expr::StrArena) -> St<'a> {
    Self { str_ar, errors: Vec::new() }
  }

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
pub(crate) fn get(st: &mut St<'_>, store: &ty::MutStore<'_>, want: ty::Ty, got: ty::Ty) {
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
    // all sets are arrays, but not all arrays are sets. so sets are a subtype of arrays, i guess.
    // also, for both sets and arrays, the element type variable is invariant in the sense of
    // subtyping sense.
    (
      &ty::Data::Array(ty::Array { elem: want, is_set: true }),
      &ty::Data::Array(ty::Array { elem: got, is_set: true }),
    )
    | (
      &ty::Data::Array(ty::Array { elem: want, is_set: false }),
      &ty::Data::Array(ty::Array { elem: got, is_set: _ }),
    ) => {
      get(st, store, want, got);
    }
    (ty::Data::Tuple(want), ty::Data::Tuple(got)) => {
      if want.elems.len() != got.elems.len() {
        st.err(error::Unify::WrongNumTupleElem(want.elems.len(), got.elems.len()));
        return;
      }
      for (&w, &g) in want.elems.iter().zip(got.elems.iter()) {
        get(st, store, w, g);
      }
    }
    // all tuples are arrays, but not all arrays are tuples. leave the want tuple, got array case
    // unhandled; it'll fall through to _ at the end. also note not all tuples are sets, so we
    // conservatively require the array to be not a set. we might be able to unify sets with known
    // sorted and dupe-free tuples but let's not.
    (&ty::Data::Array(ty::Array { elem: want, is_set: false }), ty::Data::Tuple(got)) => {
      // wanted array elem must be ALL of the got tuple elems. (like a union)
      for &g in &got.elems {
        get(st, store, want, g);
      }
    }
    (ty::Data::Object(want_obj), ty::Data::Object(got_obj)) => {
      for (&name, &w) in &want_obj.known {
        let Some(&g) = got_obj.known.get(&name) else {
          if let Some(u) = error::Unify::no_such_field(st.str_ar, got_obj, name) {
            st.err(u);
          }
          continue;
        };
        get(st, store, w, g);
      }
      if !want_obj.has_unknown && got_obj.known.len() > want_obj.known.len() {
        st.err(error::Unify::Incompatible(want, got));
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
      let parts = (want.parts(), got.parts());
      // if either has no parts (is unknown), it will unify with the other always.
      let (Some((want_params, want_ret)), Some((got_params, got_ret))) = parts else { return };
      if want_params.len() > got_params.len() {
        st.err(error::Unify::NotEnoughParams(want_params.len(), got_params.len()));
      }
      for (want, got) in want_params.iter().zip(got_params.iter()) {
        if !want.id.is_unutterable() && want.id != got.id {
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
      get(st, store, want_ret, got_ret);
    }
    // got-union MUST come before want-union
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
      st.err(error::Unify::Incompatible(want, got));
    }
    _ => st.err(error::Unify::Incompatible(want, got)),
  }
}
