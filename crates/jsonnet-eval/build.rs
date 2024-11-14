//! Generate code for calling the standard library functions.

use jsonnet_std_sig::Ty;
use quote::quote as q;
use std::collections::BTreeSet;

const JOINER: &str = "__";

fn can_mk_arm(s: &str) -> bool {
  matches!(
    s,
    "type_"
      | "isArray"
      | "isBoolean"
      | "isFunction"
      | "isNumber"
      | "isObject"
      | "isString"
      | "length"
      | "sign"
      | "max"
      | "min"
      | "pow"
      | "abs"
      | "exp"
      | "log"
      | "floor"
      | "ceil"
      | "sqrt"
      | "sin"
      | "cos"
      | "tan"
      | "asin"
      | "acos"
      | "atan"
      | "round"
      | "equals"
  )
}

fn main() {
  let unique_param_lists: BTreeSet<Vec<_>> = jsonnet_std_sig::FNS.iter().map(param_names).collect();
  let get_params = unique_param_lists.iter().map(|params| mk_get_params(params));
  let get_args = jsonnet_std_sig::FNS.iter().map(mk_get_args);
  let get_arms =
    jsonnet_std_sig::FNS.iter().filter(|func| can_mk_arm(func.name.ident())).map(mk_arm);

  let file = file!();

  let contents = q! {
    use crate::{exec, std_lib, std_lib_impl};
    use jsonnet_expr::StdFn;

    pub const _GENERATED_BY: &str = #file;

    pub(crate) fn get(
      cx: crate::Cx<'_>,
      env: &jsonnet_val::jsonnet::Env,
      positional: &[jsonnet_expr::Expr],
      named: &[(jsonnet_expr::Id, jsonnet_expr::Expr)],
      expr: jsonnet_expr::ExprMust,
      std_fn: StdFn,
    ) -> crate::error::Result<jsonnet_val::jsonnet::Val> {
      match std_fn {
        #(#get_arms)*
        _ => Err(crate::mk_todo(expr, "auto-gen'd std")),
      }
    }


    #[expect(dead_code, non_camel_case_types, non_snake_case)]
    mod params {
      use jsonnet_expr::arg::{Result, TooMany, Error, ErrorKind};
      use jsonnet_expr::{Id, Expr, ExprMust};

      #(#get_params)*
    }

    #[expect(dead_code, non_snake_case)]
    pub(crate) mod args {
      use jsonnet_expr::arg::{Result};
      use jsonnet_expr::{Id, Expr, ExprMust};
      use super::params;

      #(#get_args)*
    }
  };
  write_rs_tokens::go(contents, "generated.rs");
}

fn mk_get_args(f: &jsonnet_std_sig::Fn) -> proc_macro2::TokenStream {
  let args_struct = param_names(f).join(JOINER);
  let args_struct = ident(args_struct.as_str());
  let name = ident(f.name.ident());
  q! {
    #[doc = "# Errors"]
    #[doc = "If getting the args failed."]
    pub fn #name(
      positional: &[Expr],
      named: &[(Id, Expr)],
      expr: ExprMust,
    ) -> Result<params::#args_struct> {
      params::#args_struct::get(positional, named, expr)
    }
  }
}

fn mk_get_params(params: &[&str]) -> proc_macro2::TokenStream {
  let name = params.join(JOINER);
  let name = ident(name.as_str());
  let in_progress = quote::format_ident!("TMP{JOINER}{name}");
  let num_params = params.len();
  let ids = params.iter().map(|&param| {
    let param = ident(param);
    q! { Id::#param, }
  });
  let fields = params.iter().map(|&param| {
    let param = ident(param);
    q! { pub #param: Expr, }
  });
  let opt_fields = params.iter().map(|&param| {
    let param = ident(param);
    q! { #param: Option<Expr>, }
  });
  let init_from_positional = params.iter().map(|&param| {
    let param = ident(param);
    q! { #param: positional.next(), }
  });
  let set_from_named = params.iter().map(|&param| {
    let param = ident(param);
    q! {
      if arg_name == Id::#param {
        if in_progress.#param.is_some() {
          return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::Duplicate(arg_name) });
        }
        in_progress.#param = Some(arg);
      } else
    }
  });
  let require_vars_set = params.iter().map(|&param| {
    let param = ident(param);
    q! {
      if in_progress.#param.is_none() {
        return Err(Error { expr, kind: ErrorKind::NotDefined(Id::#param) });
      }
    }
  });
  let unwraps_unchecked = params.iter().map(|&param| {
    let param = ident(param);
    q! { #param: unsafe { in_progress.#param.unwrap_unchecked() }, }
  });
  q! {
    #[derive(Debug)]
    pub struct #name {
      #(#fields)*
    }

    struct #in_progress {
      #(#opt_fields)*
    }

    impl #name {
      const IDS: [Id; #num_params] = [
        #(#ids)*
      ];

      pub fn get(
        positional: &[Expr],
        named: &[(Id, Expr)],
        expr: ExprMust,
      ) -> Result<Self> {
        if let Some(tma) = TooMany::new(
          #name::IDS.iter().copied(),
          positional.len(),
          named.iter().map(|&(id, _)| id),
        ) {
          return Err(Error { expr, kind: ErrorKind::TooMany(tma) });
        }
        let mut positional = positional.iter().copied();
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

fn param_names(f: &jsonnet_std_sig::Fn) -> Vec<&'static str> {
  f.sig.params.iter().map(|x| x.name).collect()
}

fn mk_arm(func: &jsonnet_std_sig::Fn) -> proc_macro2::TokenStream {
  let name = ident(func.name.ident());
  let get_args = func.sig.params.iter().map(|param| {
    let name = ident(param.name);
    let conv = match param.ty {
      Ty::Any | Ty::StrOrArrNum | Ty::StrOrArrAny | Ty::NumOrNull | Ty::NumOrStr => q! {},
      Ty::True | Ty::Bool => todo!("conv param Bool"),
      Ty::Num => q! { let #name = std_lib::get_num(&#name, arguments.#name.unwrap_or(expr))?; },
      Ty::Uint => todo!("conv param Uint"),
      Ty::Str => todo!("conv param Str"),
      Ty::ArrBool => todo!("conv param ArrBool"),
      Ty::ArrNum => todo!("conv param ArrNum"),
      Ty::ArrStr => todo!("conv param ArrStr"),
      Ty::ArrKv => todo!("conv param ArrKv"),
      Ty::ArrAny => q! { let #name = std_lib::get_arr(&#name, arguments.#name.unwrap_or(expr))?; },
      Ty::Obj => todo!("conv param Obj"),
      Ty::Hof1 => todo!("conv param Hof1"),
      Ty::Hof2 => todo!("conv param Hof2"),
    };
    q! {
      let #name = exec::get(cx, env, arguments.#name)?;
      #conv
    }
  });
  let send_args = func.sig.params.iter().map(|param| {
    let name = ident(param.name);
    if matches!(param.ty, Ty::True | Ty::Bool | Ty::Num | Ty::Str) {
      q! { #name }
    } else {
      q! { &#name }
    }
  });
  let conv_res = match func.sig.ret {
    Ty::Any | Ty::StrOrArrNum | Ty::StrOrArrAny | Ty::NumOrNull | Ty::NumOrStr => q! { Ok(res) },
    Ty::True | Ty::Bool | Ty::Str => q! { Ok(res.into()) },
    Ty::Num => q! { std_lib::mk_num(res, expr) },
    Ty::Uint => q! { Ok(finite_float::Float::from(res).into()) },
    Ty::ArrBool => todo!("conv ret ArrBool"),
    Ty::ArrNum => todo!("conv ret ArrNum"),
    Ty::ArrStr => todo!("conv ret ArrStr"),
    Ty::ArrKv => todo!("conv ret ArrKv"),
    Ty::ArrAny => todo!("conv ret ArrAny"),
    Ty::Obj => todo!("conv ret Obj"),
    Ty::Hof1 => todo!("conv ret Hof1"),
    Ty::Hof2 => todo!("conv ret Hof2"),
  };
  let partial_args = if func.total {
    q! {}
  } else {
    q! { expr, cx }
  };
  let partial_conv = if func.total {
    q! {}
  } else {
    q! { ? }
  };
  q! {
    StdFn::#name => {
      let arguments = args::#name(positional, named, expr)?;
      #(#get_args)*
      let res = std_lib_impl::#name(#(#send_args,)* #partial_args) #partial_conv;
      #conv_res
    }
  }
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}
