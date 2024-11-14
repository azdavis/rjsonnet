//! Generate code for calling the standard library functions.

use quote::quote as q;
use std::collections::BTreeSet;

const JOINER: &str = "__";

fn can_mk_arm(_: &str) -> bool {
  false
}

fn main() {
  let unique_param_lists: BTreeSet<Vec<_>> = jsonnet_std_sig::FNS.iter().map(param_names).collect();
  let get_params = unique_param_lists.iter().map(|params| mk_get_params(params));
  let get_args = jsonnet_std_sig::FNS.iter().map(mk_get_args);
  let get_arms =
    jsonnet_std_sig::FNS.iter().filter(|func| can_mk_arm(func.name.ident())).map(mk_arm);

  let file = file!();

  let contents = q! {
    use jsonnet_expr::StdFn;

    pub const _GENERATED_BY: &str = #file;

    pub(crate) fn get(
      _cx: crate::Cx<'_>,
      _env: &jsonnet_val::jsonnet::Env,
      _positional: &[jsonnet_expr::Expr],
      _named: &[(jsonnet_expr::Id, jsonnet_expr::Expr)],
      expr: jsonnet_expr::ExprMust,
      std_fn: StdFn,
    ) -> crate::error::Result<jsonnet_val::jsonnet::Val> {
      match std_fn {
        #(#get_arms)*
        _ => Err(crate::mk_todo(expr, "auto-gen'd std")),
      }
    }


    #[expect(dead_code, non_camel_case_types, non_snake_case)]
    pub(crate) mod params {
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

      #[doc = "# Errors"]
      #[doc = "If getting the arguments failed."]
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

fn mk_arm(_: &jsonnet_std_sig::Fn) -> proc_macro2::TokenStream {
  todo!()
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}
