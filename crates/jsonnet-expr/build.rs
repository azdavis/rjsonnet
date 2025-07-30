//! Generate some string/identifier names.

#![expect(clippy::disallowed_methods, reason = "ok to panic in build scripts")]

use jsonnet_std_sig::S;
use quote::quote as q;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt::Write as _;

macro_rules! mk_unutterable {
  ($x: literal) => {
    S::named(concat!("$", $x), concat!($x, "_unutterable"))
  };
}

#[expect(clippy::too_many_lines)]
fn main() {
  let arg_names: BTreeSet<_> =
    jsonnet_std_sig::FNS.into_iter().flat_map(|f| f.sig.params).map(|x| x.name).collect();
  let arg_names_except_std_fn_names = {
    let std_fn_names: HashSet<_> = jsonnet_std_sig::FNS.iter().map(|f| f.name.ident()).collect();
    let mut tmp = arg_names.clone();
    tmp.retain(|x| !std_fn_names.contains(x));
    tmp
  };
  let unutterable = [
    // the same as std, but since it is unutterable, it will never be shadowed. used in desugaring.
    mk_unutterable!("std"),
    // these are used as the param names for a function f, which is itself a param for a std
    // function. it is known that this function f will always be called with only positional params,
    // never named params. so the param names for f are intensionally not utterable in user code.
    //
    // this allows user code to be more permissive. when a user makes a function g that they pass as
    // the argument value for the param f of the std function, the user may choose any param name(s)
    // for that user-written function g.
    //
    // the first one is for single-param std fn params.
    mk_unutterable!("a"),
    // these are more descriptive for multi-param std fn params.
    mk_unutterable!("acc"),
    mk_unutterable!("elem"),
    mk_unutterable!("key"),
    mk_unutterable!("value"),
    mk_unutterable!("idx"),
    // these are used for functions which do not have known parameter names, but are known via flow
    // typing to have a certain number of parameters. the first one ("a") is re-used from the single
    // param std fn param above.
    mk_unutterable!("b"),
    mk_unutterable!("c"),
    mk_unutterable!("d"),
    mk_unutterable!("e"),
    // these are used for +: fields in the desugaring
    mk_unutterable!("outerself"),
    mk_unutterable!("outersuper"),
  ];
  let builtin_identifiers = [
    S::new("std"),
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
    // needed for std fn param defaults
    S::named("\n", "newline_char"),
    S::named(": ", "colon_space"),
  ];
  let strings = || {
    std::iter::empty()
      .chain(jsonnet_std_sig::FNS.iter().map(|f| f.name))
      .chain(jsonnet_std_sig::FIELDS.iter().map(|f| f.name))
      .chain(messages.iter().copied())
  };

  let all = || {
    std::iter::empty()
      .chain(unutterable.iter().copied())
      .chain(builtin_identifiers.iter().copied())
      .chain(arg_names_except_std_fn_names.iter().map(|&x| S::new(x)))
      .chain(strings())
  };

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for s in all() {
    assert!(names.insert(s.ident()), "duplicate ident: {}", s.ident());
    assert!(contents.insert(s.content()), "duplicate content: {}", s.content());
  }

  // needed for { key: string, value: T } for std.objectKeysValues
  let key = S::new("key");
  let value = S::new("value");
  assert!(names.contains("key"));
  assert!(contents.contains("key"));
  assert!(names.contains("value"));
  assert!(contents.contains("value"));
  drop(names);
  drop(contents);

  let builtin_str = {
    let variants = all().map(|s| {
      let name = ident(s.ident());
      q! { #name, }
    });
    let as_static_str_arms = all().map(|s| {
      let name = ident(s.ident());
      let content = s.content();
      q! { Self::#name => #content, }
    });
    let from_str_arms = all().map(|s| {
      let name = ident(s.ident());
      let content = s.content();
      q! { #content => Self::#name, }
    });
    let unutterable_pats = unutterable.iter().map(|s| {
      let name = ident(s.ident());
      q! { Self::#name }
    });

    q! {
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

        pub(crate) const fn is_unutterable(self) -> bool {
          matches!(self, #(#unutterable_pats)|*)
        }
      }

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
      .chain(unutterable.iter().copied())
      .chain(builtin_identifiers.iter().copied())
      .chain(arg_names.iter().map(|&x| S::new(x)))
      .map(|s| {
        let name = ident(s.ident());
        q! { pub const #name: Self = Self::builtin(BuiltinStr::#name); }
      });

    q! {
      #[expect(non_upper_case_globals)]
      impl Id {
        #(#constants)*
      }
    }
  };

  let impl_str = {
    let tmp = strings().chain(std::iter::once(key)).chain(std::iter::once(value));
    let constants = tmp.map(|s| {
      let name = ident(s.ident());
      q! { pub const #name: Self = Self::builtin(BuiltinStr::#name); }
    });

    q! {
      #[expect(non_upper_case_globals)]
      impl Str {
        #(#constants)*
      }
    }
  };

  let std_fn = {
    let variants = jsonnet_std_sig::FNS.iter().map(|f| ident(f.name.ident()));
    let from_str_arms = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      q! { Str::#name => Self::#name, }
    });
    let as_builtin_str_arms = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      q! { Self::#name => Str::#name, }
    });
    let as_static_str_arms = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      let content = f.name.content();
      q! { Self::#name => #content, }
    });
    let doc_arms = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      let mut content = f.doc.to_owned();
      if !f.examples.is_empty() {
        content.push_str("\n\n```jsonnet\n");
        for example in f.examples.get() {
          writeln!(content, "assert {};", example.trim()).unwrap();
        }
        content.push_str("```\n");
      }
      if f.sig.params.iter().any(|p| !p.is_required()) {
        content
          .push_str("\nDefault values for optional parameters:\n\n| Name | Value |\n|--|--|\n");
        for p in f.sig.params {
          let Some(d) = p.default else { continue };
          writeln!(content, "| `{}` | `{}` |", p.name, d).unwrap();
        }
      }
      q! { Self::#name => #content, }
    });
    let mut tmp = BTreeMap::<usize, BTreeSet<&str>>::new();
    for f in jsonnet_std_sig::FNS {
      let n = f.sig.params.iter().filter(|param| param.is_required()).count();
      tmp.entry(n).or_default().insert(f.name.ident());
    }
    let required_params_count_arms = tmp.iter().map(|(&n, s)| {
      let pats = s.iter().map(|x| {
        let id = ident(x);
        q! { StdFn::#id }
      });
      q! { #(#pats)|* => #n, }
    });
    q! {
      #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
      #[expect(non_camel_case_types)]
      pub enum StdFn {
        #(#variants,)*
      }

      #[expect(clippy::too_many_lines)]
      impl StdFn {
        #[doc = "Returns a static str for this."]
        #[must_use]
        pub const fn as_static_str(self) -> &'static str {
          match self {
            #(#as_static_str_arms)*
          }
        }

        #[doc = "Returns the built-in `Str` for this."]
        #[must_use]
        pub const fn as_builtin_str(self) -> Str {
          match self {
            #(#as_builtin_str_arms)*
          }
        }

        #[doc = "Returns Markdown documentation for this."]
        #[must_use]
        pub const fn doc(self) -> &'static str {
          match self {
            #(#doc_arms)*
          }
        }

        #[doc = "Returns the number of required params for this."]
        #[must_use]
        pub const fn required_params_count(self) -> usize {
          match self {
            #(#required_params_count_arms)*
          }
        }
      }

      #[expect(clippy::too_many_lines)]
      impl TryFrom<Str> for StdFn {
        type Error = ();

        fn try_from(s: Str) -> Result<Self, Self::Error> {
          let ret = match s {
            #(#from_str_arms)*
            _ => return Err(()),
          };
          Ok(ret)
        }
      }
    }
  };

  let std_field = {
    let field_variants = jsonnet_std_sig::FIELDS.iter().map(|x| ident(x.name.ident()));
    let count = jsonnet_std_sig::FNS.len() + jsonnet_std_sig::FIELDS.len();
    let str_variant_tuples = std::iter::empty()
      .chain(jsonnet_std_sig::FNS.iter().map(|x| {
        let name = ident(x.name.ident());
        q! { (Str::#name, Self::Fn(StdFn::#name)) }
      }))
      .chain(jsonnet_std_sig::FIELDS.iter().map(|x| {
        let name = ident(x.name.ident());
        q! { (Str::#name, Self::#name) }
      }));
    let field_doc_arms = jsonnet_std_sig::FIELDS.iter().map(|x| {
      let name = ident(x.name.ident());
      let doc = x.doc;
      q! { Self::#name => #doc, }
    });
    let as_builtin_str_arms = jsonnet_std_sig::FIELDS.iter().map(|x| {
      let name = ident(x.name.ident());
      q! { Self::#name => Str::#name, }
    });
    let from_str_ifs = jsonnet_std_sig::FIELDS.iter().map(|x| {
      let name = ident(x.name.ident());
      q! {
        if s == Str::#name {
          return Ok(Self::#name);
        }
      }
    });

    q! {
      #[expect(non_camel_case_types)]
      #[derive(Debug, Clone, Copy)]
      pub enum StdField {
        #(#field_variants,)*
        Fn(StdFn),
      }

      impl StdField {
        pub const ALL: [(Str, Self); #count] = [
          #(#str_variant_tuples,)*
        ];

        #[must_use]
        pub fn doc(&self) -> &'static str {
          match self {
            #(#field_doc_arms)*
            StdField::Fn(std_fn) => std_fn.doc(),
          }
        }

        #[must_use]
        pub fn as_builtin_str(&self) -> Str {
          match self {
            #(#as_builtin_str_arms)*
            StdField::Fn(std_fn) => std_fn.as_builtin_str(),
          }
        }
      }

      impl TryFrom<Str> for StdField {
        type Error = ();

        fn try_from(s: Str) -> Result<Self, Self::Error> {
          #(#from_str_ifs)*
          match StdFn::try_from(s) {
            Ok(x) => Ok(Self::Fn(x)),
            Err(()) => Err(()),
          }
        }
      }
    }
  };

  let file = file!();

  let contents = q! {
    pub const _GENERATED_BY: &str = #file;

    use crate::{Id, Str};

    #builtin_str

    #impl_id

    #impl_str

    #std_fn

    #std_field
  };
  write_rs_tokens::go(contents, "generated.rs");
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}
