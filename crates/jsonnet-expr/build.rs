//! Generate some string/identifier names.

use jsonnet_std::{Sig, S};
use quote::{format_ident, quote};
use std::collections::{BTreeMap, BTreeSet, HashSet};

const JOINER: &str = "__";

#[expect(clippy::too_many_lines)]
fn main() {
  let arg_names = {
    let mut tmp = BTreeSet::<&str>::new();
    for f in jsonnet_std::FNS {
      match f.sig {
        Sig::Regular(params, _) => tmp.extend(params.iter().map(|x| x.name)),
        Sig::Special(params) => tmp.extend(params),
      }
    }
    tmp
  };
  let arg_names_except_std_fn_names = {
    let std_fn_names: HashSet<_> = jsonnet_std::FNS.iter().map(|f| f.name.name()).collect();
    let mut tmp = arg_names.clone();
    tmp.retain(|x| !std_fn_names.contains(x));
    tmp
  };
  let builtin_identifiers = [
    S::new("std"),
    // std_unutterable is the same as std but it has a str that cannot be written in user code as an
    // id, so it will never be shadowed. it is used in desugaring.
    S::named("$std", "std_unutterable"),
    S::named("self", "self_"),
    S::named("super", "super_"),
    S::named("$", "dollar"),
  ];
  let messages = [
    S::named("Assertion failed", "ASSERTION_FAILED"),
    S::new("array"),
    S::new("boolean"),
    S::new("function"),
    S::new("null"),
    S::new("number"),
    S::new("object"),
    S::new("string"),
  ];
  let strings = || {
    std::iter::once(S::new("thisFile"))
      .chain(jsonnet_std::FNS.iter().map(|f| f.name))
      .chain(messages.iter().copied())
  };

  let all = || {
    std::iter::empty()
      .chain(builtin_identifiers.iter().copied())
      .chain(arg_names_except_std_fn_names.iter().map(|&x| S::new(x)))
      .chain(strings())
  };

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for s in all() {
    assert!(names.insert(s.name()), "duplicate ident: {}", s.name());
    assert!(contents.insert(s.content()), "duplicate content: {}", s.content());
    assert!(!s.name().contains(JOINER));
  }
  drop(names);
  drop(contents);

  let builtin_str = {
    let variants = all().map(|s| {
      let name = format_ident!("{}", s.name());
      quote! { #name, }
    });
    let as_static_str_arms = all().map(|s| {
      let name = format_ident!("{}", s.name());
      let content = s.content();
      quote! { Self::#name => #content, }
    });
    let from_str_arms = all().map(|s| {
      let name = format_ident!("{}", s.name());
      let content = s.content();
      quote! { #content => Self::#name, }
    });

    quote! {
      #[expect(non_camel_case_types)]
      #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
      pub(crate) enum BuiltinStr {
        #(#variants)*
      }

      impl BuiltinStr {
        #[expect(clippy::too_many_lines)]
        pub(crate) const fn as_static_str(self) -> &'static str {
          match self {
            #(#as_static_str_arms)*
          }
        }
      }

      // TODO: make this faster with phf?
      impl std::str::FromStr for BuiltinStr {
        type Err = NotBuiltinStr;

        #[expect(clippy::too_many_lines)]
        fn from_str(s: &str) -> Result<Self, Self::Err> {
          let ret = match s {
            #(#from_str_arms)*
            _ => return Err(NotBuiltinStr(())),
          };
          Ok(ret)
        }
      }

      #[doc = "A type that serves as a witness that a `Str` was not a `BuiltinStr`."]
      pub(crate) struct NotBuiltinStr(());

      impl NotBuiltinStr {
        #[doc = "NOTE: only call this when we know the string just came from a string arena, "]
        #[doc = "and is therefore known to be NOT a builtin"]
        pub(crate) fn from_str_arena() -> Self {
          Self(())
        }
      }
    }
  };

  let impl_id = {
    let constants = std::iter::empty()
      .chain(builtin_identifiers.iter().copied().map(|x| (x, true)))
      .chain(arg_names.iter().map(|&x| (S::new(x), false)))
      .map(|(s, is_pub)| {
        let name = format_ident!("{}", s.name());
        let vis = if is_pub {
          quote! { pub }
        } else {
          quote! {}
        };
        quote! { #vis const #name: Self = Self::builtin(BuiltinStr::#name); }
      });

    quote! {
      #[expect(non_upper_case_globals)]
      impl Id {
        #(#constants)*
      }
    }
  };

  let impl_str = {
    let constants = strings().map(|s| {
      let name = format_ident!("{}", s.name());
      quote! { pub const #name: Self = Self::builtin(BuiltinStr::#name); }
    });

    quote! {
      #[expect(non_upper_case_globals)]
      impl Str {
        #(#constants)*
      }
    }
  };

  let std_fn = {
    let variants = jsonnet_std::FNS.iter().map(|f| format_ident!("{}", f.name.name()));
    let count = jsonnet_std::FNS.len();
    let str_variant_tuples = jsonnet_std::FNS.iter().map(|f| {
      let name = format_ident!("{}", f.name.name());
      quote! { (Str::#name, Self::#name) }
    });
    let as_static_str_arms = jsonnet_std::FNS.iter().map(|f| {
      let name = format_ident!("{}", f.name.name());
      let content = f.name.content();
      quote! { Self::#name => #content, }
    });
    let unique_params_lens = {
      let mut tmp = BTreeMap::<usize, BTreeSet<&str>>::new();
      for f in &jsonnet_std::FNS {
        let params_len = match f.sig {
          Sig::Regular(xs, _) => xs.len(),
          Sig::Special(xs) => xs.len(),
        };
        tmp.entry(params_len).or_default().insert(f.name.name());
      }
      tmp
    };
    let params_len_arms = unique_params_lens.iter().map(|(&len, names)| {
      let pats = names.iter().map(|name| {
        let name = format_ident!("{name}");
        quote! { | Self::#name }
      });
      quote! { #(#pats)* => #len, }
    });
    let unique_param_lists: BTreeSet<Vec<_>> = jsonnet_std::FNS.iter().map(param_names).collect();
    let get_params = unique_param_lists.iter().map(|params| mk_get_params(params));
    let get_args = jsonnet_std::FNS.iter().map(mk_get_args);
    quote! {
      #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
      #[expect(non_camel_case_types)]
      pub enum StdFn {
        #(#variants,)*
      }

      impl StdFn {
        pub const ALL: [(Str, Self); #count] = [
          #(#str_variant_tuples,)*
        ];

        #[must_use]
        #[expect(clippy::too_many_lines)]
        pub fn params_len(&self) -> usize {
          match self {
            #(#params_len_arms)*
          }
        }

        #[doc = "Returns a static str for this."]
        #[expect(clippy::too_many_lines)]
        #[must_use]
        pub const fn as_static_str(self) -> &'static str {
          match self {
            #(#as_static_str_arms)*
          }
        }
      }

      pub mod std_fn {
        #[expect(non_camel_case_types, non_snake_case)]
        pub mod params {
          use crate::arg::{Result, TooMany, Error, ErrorKind};
          use crate::{Id, Expr, ExprMust};

          #(#get_params)*
        }

        #[expect(non_snake_case)]
        pub mod args {
          use crate::arg::{Result};
          use crate::{Id, Expr, ExprMust};
          use super::params;

          #(#get_args)*
        }
      }
    }
  };

  let file = file!();

  let contents = quote! {
    pub const _GENERATED_BY: &str = #file;

    use crate::{Id, Str};

    #builtin_str

    #impl_id

    #impl_str

    #std_fn
  };
  write_rs_tokens::go(contents, "generated.rs");
}

fn mk_get_args(f: &jsonnet_std::Fn) -> proc_macro2::TokenStream {
  let args_struct = param_names(f).join(JOINER);
  let args_struct = format_ident!("{args_struct}");
  let name = format_ident!("{}", f.name.name());
  quote! {
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
  let name = format_ident!("{name}");
  let in_progress = format_ident!("TMP{JOINER}{name}");
  let num_params = params.len();
  let ids = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! { Id::#param, }
  });
  let fields = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! { pub #param: Expr, }
  });
  let opt_fields = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! { #param: Option<Expr>, }
  });
  let init_from_positional = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! { #param: positional.next(), }
  });
  let set_from_named = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! {
      if arg_name == Id::#param {
        if in_progress.#param.is_some() {
          return Err(Error { expr: arg.unwrap_or(expr), kind: ErrorKind::Duplicate(arg_name) });
        }
        in_progress.#param = Some(arg);
      } else
    }
  });
  let require_vars_set = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! {
      if in_progress.#param.is_none() {
        return Err(Error { expr, kind: ErrorKind::NotDefined(Id::#param) });
      }
    }
  });
  let unwraps_unchecked = params.iter().map(|&param| {
    let param = format_ident!("{param}");
    quote! { #param: unsafe { in_progress.#param.unwrap_unchecked() }, }
  });
  quote! {
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

fn param_names(f: &jsonnet_std::Fn) -> Vec<&'static str> {
  match f.sig {
    Sig::Regular(xs, _) => xs.iter().map(|x| x.name).collect(),
    Sig::Special(xs) => xs.to_vec(),
  }
}
