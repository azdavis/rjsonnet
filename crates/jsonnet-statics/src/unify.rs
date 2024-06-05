//! Unification.

// TODO remove
#![allow(unused)]

use crate::ty;
use jsonnet_expr::Prim;

enum Error {
  Incompatible,
}

struct St {
  errors: Vec<Error>,
}

impl St {
  fn err(&mut self, e: Error) {
    self.errors.push(e);
  }
}

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
    (ty::Data::Object(want), ty::Data::Object(got)) => todo!("object types"),
    (ty::Data::Fn(_), ty::Data::Fn(_)) => todo!("function types"),
    (ty::Data::Meta(m), ty) | (ty, ty::Data::Meta(m)) => {
      if let ty::Data::Meta(m2) = ty {
        if m == m2 {
          return;
        }
      }
      todo!("unify m with ty, setting a subst somewhere")
    }
    (ty::Data::Or(or), ty) => todo!("or types"),
    _ => st.err(Error::Incompatible),
  }
}
