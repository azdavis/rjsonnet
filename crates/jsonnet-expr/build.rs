//! Generate some string/identifier names.

use quote::{format_ident, quote};
use std::collections::{BTreeMap, BTreeSet, HashSet};

const JOINER: &str = "__";

#[derive(Debug, Clone, Copy)]
struct S {
  name: &'static str,
  content: &'static str,
}

impl S {
  fn named(content: &'static str, name: &'static str) -> S {
    S { name, content }
  }

  fn new(content: &'static str) -> S {
    S { name: content, content }
  }
}

#[expect(clippy::too_many_lines)]
fn main() {
  let std_fns = [
    (S::new("extVar"), &["x"][..]),
    (S::named("type", "type_"), &["x"]),
    (S::new("isArray"), &["v"]),
    (S::new("isBoolean"), &["v"]),
    (S::new("isFunction"), &["v"]),
    (S::new("isNumber"), &["v"]),
    (S::new("isObject"), &["v"]),
    (S::new("isString"), &["v"]),
    (S::new("length"), &["x"]),
    (S::new("get"), &["o", "f", "default", "inc_hidden"]),
    (S::new("objectHas"), &["o", "f"]),
    (S::new("objectFields"), &["o"]),
    (S::new("objectValues"), &["o"]),
    (S::new("objectKeysValues"), &["o"]),
    (S::new("objectHasAll"), &["o", "f"]),
    (S::new("objectFieldsAll"), &["o"]),
    (S::new("objectValuesAll"), &["o"]),
    (S::new("objectKeysValuesAll"), &["o"]),
    (S::new("prune"), &["a"]),
    (S::new("mapWithKey"), &["func", "obj"]),
    (S::new("abs"), &["n"]),
    (S::new("sign"), &["n"]),
    (S::new("max"), &["a", "b"]),
    (S::new("min"), &["a", "b"]),
    (S::new("pow"), &["x", "n"]),
    (S::new("exp"), &["x"]),
    (S::new("log"), &["x"]),
    (S::new("exponent"), &["x"]),
    (S::new("mantissa"), &["x"]),
    (S::new("floor"), &["x"]),
    (S::new("ceil"), &["x"]),
    (S::new("sqrt"), &["x"]),
    (S::new("sin"), &["x"]),
    (S::new("cos"), &["x"]),
    (S::new("tan"), &["x"]),
    (S::new("asin"), &["x"]),
    (S::new("acos"), &["x"]),
    (S::new("atan"), &["x"]),
    (S::new("round"), &["x"]),
    (S::named("mod", "mod_"), &["a", "b"]),
    (S::new("clamp"), &["x", "minVal", "maxVal"]),
    (S::new("assertEqual"), &["a", "b"]),
    (S::new("toString"), &["a"]),
    (S::new("codepoint"), &["str"]),
    (S::new("char"), &["n"]),
    (S::new("substr"), &["str", "from", "len"]),
    (S::new("findSubstr"), &["pat", "str"]),
    (S::new("startsWith"), &["a", "b"]),
    (S::new("endsWith"), &["a", "b"]),
    (S::new("stripChars"), &["str", "chars"]),
    (S::new("lstripChars"), &["str", "chars"]),
    (S::new("rstripChars"), &["str", "chars"]),
    (S::new("split"), &["str", "c"]),
    (S::new("splitLimit"), &["str", "c", "maxsplits"]),
    (S::new("splitLimitR"), &["str", "c", "maxsplits"]),
    (S::new("strReplace"), &["str", "from", "to"]),
    (S::new("isEmpty"), &["str"]),
    (S::new("asciiUpper"), &["str"]),
    (S::new("asciiLower"), &["str"]),
    (S::new("stringChars"), &["str"]),
    (S::new("format"), &["str", "vals"]),
    (S::new("escapeStringBash"), &["str"]),
    (S::new("escapeStringDollars"), &["str"]),
    (S::new("escapeStringJson"), &["str"]),
    (S::new("escapeStringPython"), &["str"]),
    (S::new("escapeStringXml"), &["str"]),
    (S::new("parseInt"), &["str"]),
    (S::new("parseOctal"), &["str"]),
    (S::new("parseHex"), &["str"]),
    (S::new("parseJson"), &["str"]),
    (S::new("parseYaml"), &["str"]),
    (S::new("encodeUTF8"), &["str"]),
    (S::new("decodeUTF8"), &["arr"]),
    (S::new("manifestIni"), &["ini"]),
    (S::new("manifestPython"), &["v"]),
    (S::new("manifestPythonVars"), &["conf"]),
    (S::new("manifestJsonEx"), &["value", "indent", "newline", "key_val_sep"]),
    (S::new("manifestJsonMinified"), &["value"]),
    (S::new("manifestYamlDoc"), &["value", "indent_array_in_object", "quote_keys"]),
    (
      S::new("manifestYamlStream"),
      &["value", "indent_array_in_object", "c_document_end", "quote_keys"],
    ),
    (S::new("manifestXmlJsonml"), &["value"]),
    (S::new("manifestTomlEx"), &["toml", "indent"]),
    (S::new("makeArray"), &["sz", "func"]),
    (S::new("member"), &["arr", "x"]),
    (S::new("count"), &["arr", "x"]),
    (S::new("find"), &["value", "arr"]),
    (S::new("map"), &["func", "arr"]),
    (S::new("mapWithIndex"), &["func", "arr"]),
    (S::new("filterMap"), &["filter_func", "map_func", "arr"]),
    (S::new("flatMap"), &["func", "arr"]),
    (S::new("filter"), &["func", "arr"]),
    (S::new("foldl"), &["func", "arr", "init"]),
    (S::new("foldr"), &["func", "arr", "init"]),
    (S::new("range"), &["from", "to"]),
    (S::new("repeat"), &["what", "count"]),
    (S::new("slice"), &["indexable", "index", "end", "step"]),
    (S::new("join"), &["sep", "arr"]),
    (S::new("lines"), &["arr"]),
    (S::new("flattenArrays"), &["arr"]),
    (S::new("reverse"), &["arrs"]),
    (S::new("sort"), &["arr", "keyF"]),
    (S::new("uniq"), &["arr", "keyF"]),
    (S::new("all"), &["arr"]),
    (S::new("any"), &["arr"]),
    (S::new("sum"), &["arr"]),
    (S::new("set"), &["arr", "keyF"]),
    (S::new("setInter"), &["a", "b", "keyF"]),
    (S::new("setUnion"), &["a", "b", "keyF"]),
    (S::new("setDiff"), &["a", "b", "keyF"]),
    (S::new("setMember"), &["x", "arr", "keyF"]),
    (S::new("base64"), &["input"]),
    (S::new("base64DecodeBytes"), &["str"]),
    (S::new("base64Decode"), &["str"]),
    (S::new("md5"), &["s"]),
    (S::new("xor"), &["x", "y"]),
    (S::new("xnor"), &["x", "y"]),
    (S::new("mergePatch"), &["target", "patch"]),
    (S::new("trace"), &["str", "rest"]),
    // alluded to in the spec but not mentioned on the std lib page
    (S::new("cmp"), &["a", "b"]),
    (S::new("equals"), &["a", "b"]),
    (S::new("objectHasEx"), &["o", "f"]),
  ];
  let arg_names: BTreeSet<_> = std_fns.iter().flat_map(|&(_, xs)| xs).copied().collect();
  let arg_names_except_std_fn_names = {
    let std_fn_names: HashSet<_> = std_fns.iter().map(|&(S { name, .. }, _)| name).collect();
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
      .chain(std_fns.iter().map(|&(s, _)| s))
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
  for S { name, content } in all() {
    assert!(names.insert(name), "duplicate name: {name}",);
    assert!(contents.insert(content), "duplicate content: {content}",);
    assert!(!name.contains(JOINER));
  }
  drop(names);
  drop(contents);

  let builtin_str = {
    let variants = all().map(|S { name, .. }| {
      let name = format_ident!("{name}");
      quote! { #name, }
    });
    let as_static_str_arms = all().map(|S { name, content }| {
      let name = format_ident!("{name}");
      quote! { Self::#name => #content, }
    });
    let from_str_arms = all().map(|S { name, content }| {
      let name = format_ident!("{name}");
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
      .map(|(S { name, .. }, is_pub)| {
        let name = format_ident!("{name}");
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
    let constants = strings().map(|S { name, .. }| {
      let name = format_ident!("{name}");
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
    let variants = std_fns.iter().map(|&(S { name, .. }, _)| format_ident!("{name}"));
    let count = std_fns.len();
    let str_variant_tuples = std_fns.iter().map(|&(S { name, .. }, _)| {
      let name = format_ident!("{name}");
      quote! { (Str::#name, Self::#name) }
    });
    let unique_params_lens = {
      let mut tmp = BTreeMap::<usize, BTreeSet<&str>>::new();
      for &(S { name, .. }, params) in &std_fns {
        tmp.entry(params.len()).or_default().insert(name);
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
    let unique_param_lists: BTreeSet<_> = std_fns.iter().map(|&(_, params)| params).collect();
    let get_params = unique_param_lists.iter().map(|&params| mk_get_params(params));
    let get_args = std_fns.iter().map(|&(S { name, .. }, params)| mk_get_args(name, params));
    quote! {
      #[derive(Debug, Clone, Copy)]
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

fn mk_get_args(name: &str, params: &[&str]) -> proc_macro2::TokenStream {
  let args_struct = params.join(JOINER);
  let args_struct = format_ident!("{args_struct}");
  let name = format_ident!("{name}");
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
