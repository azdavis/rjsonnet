//! Generate code for calling the standard library functions.

#![expect(clippy::disallowed_methods, reason = "ok to panic in build scripts")]

use jsonnet_std_sig::{Param, Ty};
use quote::quote as q;
use std::collections::{BTreeMap, BTreeSet};

fn main() {
  let unique_param_lists: BTreeMap<_, _> = {
    let tmp: BTreeSet<_> = jsonnet_std_sig::FNS.iter().map(|x| x.sig.params).collect();
    tmp.into_iter().enumerate().map(|(idx, ps)| (ps, idx)).collect()
  };
  let get_params = unique_param_lists.iter().map(|(&ps, &idx)| mk_get_params(idx, ps));
  let call_std_arms = jsonnet_std_sig::FNS.iter().filter(|func| func.implemented).map(|func| {
    let &idx =
      unique_param_lists.get(func.sig.params).expect("params list should contain all params");
    mk_call_std_arm(idx, func)
  });

  let file = file!();

  let contents = q! {
    use crate::{exec, util, std_lib};
    use jsonnet_expr::StdFn;

    pub const _GENERATED_BY: &str = #file;

    #[allow(non_snake_case)]
    pub(crate) fn call_std(
      cx: &mut crate::Cx<'_>,
      env: &jsonnet_val::jsonnet::Env,
      pos: &[jsonnet_expr::Expr],
      named: &[(jsonnet_expr::Id, jsonnet_expr::Expr)],
      expr: jsonnet_expr::ExprMust,
      std_fn: StdFn,
    ) -> crate::error::Result<jsonnet_val::jsonnet::Val> {
      match std_fn {
        #(#call_std_arms)*
        _ => Err(crate::mk_todo(expr, std_fn.as_static_str())),
      }
    }


    #[expect(dead_code, non_snake_case)]
    mod params {
      use jsonnet_expr::arg::{Result, TooMany, Error, ErrorKind};
      use jsonnet_expr::{Id, Expr, ExprMust};

      #(#get_params)*
    }
  };
  write_rs_tokens::go(contents, "generated.rs");
}

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
    q! { pub(crate) #param: Expr, }
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
          return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::Duplicate(arg_name) });
        }
        in_progress.#param = Some(arg);
      } else
    }
  });
  let require_vars_set = params.iter().map(|param| {
    let param = ident(param.name);
    q! {
      if in_progress.#param.is_none() {
        return Err(Error { expr, kind: ErrorKind::NotDefined(Id::#param) });
      }
    }
  });
  let unwraps_unchecked = params.iter().map(|param| {
    let param = ident(param.name);
    q! { #param: unsafe { in_progress.#param.unwrap_unchecked() }, }
  });
  q! {
    pub(crate) struct #name {
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
      ) -> Result<Self> {
        if let Some(tma) = TooMany::new(
          #name::IDS.iter().copied(),
          pos.len(),
          named.iter().map(|&(id, _)| id),
        ) {
          return Err(Error { expr, kind: ErrorKind::TooMany(tma) });
        }
        let mut pos = pos.iter().copied();
        let mut in_progress = #in_progress {
          #(#init_from_positional)*
        };
        for &(arg_name, arg) in named {
          #(#set_from_named)* {
            return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::NotRequested(arg_name) });
          }
        }
        #(#require_vars_set)*
        #[expect(unsafe_code)]
        Ok(Self { #(#unwraps_unchecked)* })
      }
    }
  }
}

/// NOTE: there is an assumption that the param names for std fns (e.g. `x`, `arr`) will not clash
/// with the param names and local vars in the actual rust function (e.g. `env`, `args`)
fn mk_call_std_arm(idx: usize, func: &jsonnet_std_sig::Fn) -> proc_macro2::TokenStream {
  let name = ident(func.name.ident());
  let get_args = func.sig.params.iter().map(|param| {
    let name = ident(param.name);
    let conv = match param.ty {
      Ty::Any | Ty::StrOrArrNum | Ty::StrOrArrAny | Ty::NumOrNull | Ty::NumOrStr => q! {},
      Ty::True | Ty::Bool => {
        q! { let #name = util::get_bool(&#name, args.#name.unwrap_or(expr))?; }
      }
      Ty::Num => q! { let #name = util::get_num(&#name, args.#name.unwrap_or(expr))?; },
      Ty::Uint => q! {
        let #name = util::get_num(&#name, args.#name.unwrap_or(expr))?;
        let #name = util::get_uint(#name, args.#name.unwrap_or(expr))?;
      },
      Ty::Str => q! {
        let #name = util::get_str(&#name, args.#name.unwrap_or(expr))?;
        let #name = cx.str_ar.get(#name).to_owned();
        let #name = #name.as_str();
      },
      Ty::StrInterned => {
        q! { let #name = util::get_str(&#name, args.#name.unwrap_or(expr))?; }
      }
      Ty::ArrAny | Ty::SetAny => {
        q! { let #name = util::get_arr(&#name, args.#name.unwrap_or(expr))?; }
      }
      Ty::ArrBool => todo!("conv param ArrBool"),
      Ty::ArrNum => todo!("conv param ArrNum"),
      Ty::ArrStr => todo!("conv param ArrStr"),
      Ty::ArrKv => todo!("conv param ArrKv"),
      Ty::Obj => q! { let #name = util::get_obj(&#name, args.#name.unwrap_or(expr))?; },
      Ty::Fn1 => todo!("conv param Fn1"),
      Ty::FnAccElem | Ty::FnKv | Ty::FnIdxElem => todo!("conv param Fn2"),
    };
    q! {
      let #name = exec::get(cx, env, args.#name)?;
      #conv
    }
  });
  let send_args = func.sig.params.iter().map(|param| {
    let name = ident(param.name);
    let needs_borrow = matches!(
      param.ty,
      Ty::Any | Ty::StrOrArrNum | Ty::StrOrArrAny | Ty::NumOrNull | Ty::NumOrStr
    );
    if needs_borrow {
      q! { &#name }
    } else {
      q! { #name }
    }
  });
  let conv_res = match func.sig.ret {
    Ty::Any | Ty::StrOrArrAny | Ty::StrOrArrNum | Ty::NumOrNull | Ty::NumOrStr => q! { Ok(res) },
    Ty::True | Ty::Bool | Ty::StrInterned => q! { Ok(res.into()) },
    Ty::Str => q! { Ok(util::mk_str(cx.str_ar, res)) },
    Ty::Num => q! { util::mk_num(res, expr) },
    Ty::Uint => q! { Ok(finite_float::Float::from(res).into()) },
    Ty::ArrAny | Ty::SetAny => todo!("conv ret ArrAny/SetAny"),
    Ty::ArrBool => todo!("conv ret ArrBool"),
    Ty::ArrNum => todo!("conv ret ArrNum"),
    Ty::ArrStr => todo!("conv ret ArrStr"),
    Ty::ArrKv => todo!("conv ret ArrKv"),
    Ty::Obj => todo!("conv ret Obj"),
    Ty::Fn1 | Ty::FnAccElem | Ty::FnKv | Ty::FnIdxElem => unreachable!("will not ret a Fn"),
  };
  let (partial_args, partial_conv) =
    if func.total { (q! {}, q! {}) } else { (q! { expr, cx, }, q! { ? }) };
  let args_struct = param_struct(idx);
  q! {
    StdFn::#name => {
      let args = params::#args_struct::new(pos, named, expr)?;
      #(#get_args)*
      let res = std_lib::#name(#(#send_args,)* #partial_args) #partial_conv;
      #conv_res
    }
  }
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}

fn param_struct(idx: usize) -> proc_macro2::Ident {
  quote::format_ident!("P{idx}")
}
