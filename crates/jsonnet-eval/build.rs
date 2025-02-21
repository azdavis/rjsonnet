//! Generate code for calling the standard library functions.

#![expect(clippy::disallowed_methods, reason = "ok to panic in build scripts")]

use jsonnet_std_sig::{Param, ParamDefault, Ty};
use quote::quote as q;
use std::collections::{BTreeMap, BTreeSet};

fn main() {
  let all_params: BTreeMap<_, _> = {
    let tmp: BTreeSet<_> =
      jsonnet_std_sig::FNS.iter().filter(|func| func.implemented).map(|x| x.sig.params).collect();
    tmp.into_iter().enumerate().map(|(idx, ps)| (ps, idx)).collect()
  };
  let get_params = all_params.iter().map(|(&ps, &idx)| mk_get_params(idx, ps));
  let fns = jsonnet_std_sig::FNS.iter().filter(|func| func.implemented).map(|func| {
    let &idx = all_params.get(func.sig.params).expect("all_params should contain the params");
    let name = ident(func.name.ident());
    let alias = param_struct(idx);
    q! { pub(crate) type #name = super::raw::#alias; }
  });

  let file = file!();

  let contents = q! {
    pub const _GENERATED_BY: &str = #file;

    #[expect(non_snake_case)]
    pub(crate) mod raw {
      use crate::{exec, util, Cx};
      use always::always;
      use jsonnet_expr::arg::{TooMany, Error, ErrorKind};
      use jsonnet_expr::{Id, Expr, ExprData, ExprMust, Prim, Str};
      use jsonnet_val::jsonnet::{Array, Env, Fn, Object, Val};

      #(#get_params)*
    }

    #[expect(non_camel_case_types)]
    pub(crate) mod fns {
      #(#fns)*
    }
  };
  write_rs_tokens::go(contents, "generated.rs");
}

#[expect(clippy::too_many_lines)]
fn mk_get_params(index: usize, params: &[Param]) -> proc_macro2::TokenStream {
  let name = param_struct(index);
  let in_progress = quote::format_ident!("T{index}");
  let num_params = params.len();
  let ids = params.iter().map(|param| {
    let param = ident(param.name);
    q! { Id::#param, }
  });
  let fields = params.iter().map(|param| {
    let param_name = ident(param.name);
    let ty = if param.default.is_some() {
      q! { Option<Expr> }
    } else {
      q! { Expr }
    };
    q! { #param_name: #ty, }
  });
  let opt_fields = params.iter().map(|param| {
    let param = ident(param.name);
    q! { #param: Option<Expr>, }
  });
  let init_from_positional = params.iter().map(|param| {
    let param = ident(param.name);
    q! { #param: pos.next(), }
  });
  let set_from_named = params.iter().map(|param| {
    let param = ident(param.name);
    q! {
      if arg_name == Id::#param {
        if in_progress.#param.is_some() {
          return Err(Error {
            expr: arg.unwrap_or(expr),
            kind: ErrorKind::Duplicate(arg_name),
          }.into());
        }
        in_progress.#param = Some(arg);
      } else
    }
  });
  let require_vars_set = params.iter().map(|param| {
    let param_name = ident(param.name);
    if param.default.is_some() {
      q! {}
    } else {
      q! {
        if in_progress.#param_name.is_none() {
          return Err(Error { expr, kind: ErrorKind::NotDefined(Id::#param_name) }.into());
        }
      }
    }
  });
  let field_sets = params.iter().map(|param| {
    let param_name = ident(param.name);
    let set = if param.default.is_some() {
      q! { in_progress.#param_name }
    } else {
      q! { unsafe { in_progress.#param_name.unwrap_unchecked() } }
    };
    q! { #param_name: #set, }
  });
  let getters = params.iter().map(|param| {
    let name = ident(param.name);
    let (ret_ty, conv) = match param.ty {
      Ty::Any | Ty::StrOrArrNum | Ty::StrOrArrAny | Ty::NumOrNull | Ty::NumOrStr => (
        q!(Val),
        q! {
          let _ = expr;
          Ok(ret)
        },
      ),
      Ty::True | Ty::Bool => (q!(bool), q! { util::get_bool(ret, expr) }),
      Ty::Num => (q!(f64), q! { util::get_num(ret, expr) }),
      Ty::Uint => (
        q!(usize),
        q! {
          let ret = util::get_num(ret, expr)?;
          util::get_uint(ret, expr)
        },
      ),
      Ty::Str | Ty::StrInterned => (q!(Str), q! { util::get_str(ret, expr) }),
      Ty::ArrAny | Ty::SetAny => (q!(Array), q! { util::get_arr(ret, expr) }),
      Ty::ArrBool => todo!("conv param ArrBool"),
      Ty::ArrNum => todo!("conv param ArrNum"),
      Ty::ArrStr | Ty::SetStr => todo!("conv param ArrStr/SetStr"),
      Ty::ArrKv => todo!("conv param ArrKv"),
      Ty::Obj => (q! {Object}, q! { util::get_obj(ret, expr) }),
      Ty::Fn1 | Ty::FnAccElem | Ty::FnKv | Ty::FnIdxElem => {
        (q! {Fn}, q! { util::get_fn(ret, expr) })
      }
    };
    let to_eval = if let Some(x) = param.default {
      let ts = get_param_default(x);
      q! {
        if let Some(x) = self.#name {
          x
        } else {
          let Some(exprs) = cx.exprs.get_mut(&env.path()) else {
            always!(false, "should have this paths's exprs");
            return Err(crate::error::Error::NoPath(env.path()));
          };
          let ar = &mut exprs.ar;
          #ts
        }
      }
    } else {
      q! { self.#name }
    };
    q! {
      pub(crate) fn #name(&self, cx: &mut Cx<'_>, env: &Env) -> crate::error::Result<#ret_ty> {
        let to_eval = #to_eval;
        let expr = #to_eval.unwrap_or(self.expr);
        let ret = exec::get(cx, env, to_eval)?;
        #conv
      }
    }
  });
  q! {
    pub(crate) struct #name {
      expr: ExprMust,
      #(#fields)*
    }

    struct #in_progress {
      #(#opt_fields)*
    }

    impl #name {
      const IDS: [Id; #num_params] = [
        #(#ids)*
      ];

      pub(crate) fn new(
        pos: &[Expr],
        named: &[(Id, Expr)],
        expr: ExprMust,
      ) -> crate::error::Result<Self> {
        if let Some(tm) = TooMany::new(
          #name::IDS.iter().copied(),
          pos.len(),
          named.iter().map(|&(id, _)| id),
        ) {
          return Err(jsonnet_expr::arg::Error {
            expr,
            kind: ErrorKind::TooMany(tm),
          }.into());
        }
        let mut pos = pos.iter().copied();
        let mut in_progress = #in_progress {
          #(#init_from_positional)*
        };
        for &(arg_name, arg) in named {
          #(#set_from_named)* {
            return Err(Error {
              expr: arg.unwrap_or(expr),
              kind: ErrorKind::NotRequested(arg_name)
            }.into());
          }
        }
        #(#require_vars_set)*
        #[expect(unsafe_code)]
        Ok(Self { expr, #(#field_sets)* })
      }

      #(#getters)*
    }
  }
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}

fn param_struct(idx: usize) -> proc_macro2::Ident {
  quote::format_ident!("P{idx}")
}

fn get_param_default(pd: ParamDefault) -> proc_macro2::TokenStream {
  match pd {
    ParamDefault::IdentityFn => q! {
      let body = Some(ar.alloc(ExprData::Id(Id::a_unutterable)));
      Some(ar.alloc(ExprData::Fn { params: vec![(Id::a_unutterable, None)], body }))
    },
    ParamDefault::Null => q! {
      Some(ar.alloc(ExprData::Prim(Prim::Null)))
    },
    ParamDefault::True => q! {
      Some(ar.alloc(ExprData::Prim(Prim::Bool(true))))
    },
    ParamDefault::False => q! {
      Some(ar.alloc(ExprData::Prim(Prim::Bool(false))))
    },
    ParamDefault::NewlineChar => q! {
      Some(ar.alloc(ExprData::Prim(Prim::String(Str::newline_char))))
    },
    ParamDefault::ColonSpace => q! {
      Some(ar.alloc(ExprData::Prim(Prim::String(Str::colon_space))))
    },
  }
}
