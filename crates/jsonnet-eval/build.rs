//! Generate code for calling the standard library functions.

#![expect(clippy::disallowed_methods, reason = "ok to panic in build scripts")]

use jsonnet_std_sig::{Param, Ty};
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
      use jsonnet_expr::arg::{TooMany, Error, ErrorKind};
      use jsonnet_expr::{Id, Expr, ExprMust, Str};
      use jsonnet_val::jsonnet::{Env, Object, Array, Val};

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
    let param = ident(param.name);
    q! { #param: Expr, }
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
    let param = ident(param.name);
    q! {
      if in_progress.#param.is_none() {
        return Err(Error { expr, kind: ErrorKind::NotDefined(Id::#param) }.into());
      }
    }
  });
  let unwraps_unchecked = params.iter().map(|param| {
    let param = ident(param.name);
    q! { #param: unsafe { in_progress.#param.unwrap_unchecked() }, }
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
      Ty::ArrStr => todo!("conv param ArrStr"),
      Ty::ArrKv => todo!("conv param ArrKv"),
      Ty::Obj => (q! {Object}, q! { util::get_obj(ret, expr) }),
      Ty::Fn1 => todo!("conv param Fn1"),
      Ty::FnAccElem | Ty::FnKv | Ty::FnIdxElem => todo!("conv param Fn2"),
    };
    q! {
      pub(crate) fn #name(&self, cx: &mut Cx<'_>, env: &Env) -> crate::error::Result<#ret_ty> {
        let expr = self.#name.unwrap_or(self.expr);
        let ret = exec::get(cx, env, self.#name)?;
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
        Ok(Self { expr, #(#unwraps_unchecked)* })
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
