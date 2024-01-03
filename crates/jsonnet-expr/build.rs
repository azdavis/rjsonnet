//! Generate some string/identifier names.

use quote::{format_ident, quote};
use std::collections::HashSet;

#[allow(clippy::too_many_lines)]
fn main() {
  let identifiers = [
    ("STD", "std"),
    // STD_UNUTTERABLE is the same as STD but it has a str that cannot be written in user code as an
    // id, so it will never be shadowed. it is used in desugaring.
    ("STD_UNUTTERABLE", "$std"),
    ("SELF", "self"),
    ("SUPER", "super"),
    // OUTER_SELF and OUTER_SUPER are also unutterable and are used in the desugaring.
    ("OUTER_SELF", "$outerself"),
    ("OUTER_SUPER", "$outersuper"),
    ("DOLLAR", "$"),
  ];
  let std_fns = [
    ("CMP", "cmp"),
    ("EQUALS", "equals"),
    ("JOIN", "join"),
    ("LENGTH", "length"),
    ("MAKE_ARRAY", "makeArray"),
    ("MOD", "mod"),
    ("OBJECT_HAS_EX", "objectHasEx"),
    ("SLICE", "slice"),
  ];
  let messages = [
    ("ASSERTION_FAILED", "Assertion failed"),
    ("PARAMETER_NOT_BOUND", "Parameter not bound"),
    ("TODO", "TODO"),
  ];
  let strings = || {
    std::iter::empty()
      .chain(std::iter::once(&("THIS_FILE", "thisFile")))
      .chain(std_fns.iter())
      .chain(messages.iter())
  };

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for &(name, content) in identifiers.iter().chain(strings()) {
    assert!(names.insert(name), "duplicate name: {name}");
    assert!(contents.insert(content), "duplicate content: {content}");
  }
  drop(names);
  drop(contents);

  let impl_str_idx_and_arena = {
    let str_idx_constants = std::iter::empty()
      .chain(identifiers.iter().map(|x| (x, false)))
      .chain(strings().map(|x| (x, true)))
      .enumerate()
      .map(|(idx, (&(name, _), is_pub))| {
        let name = format_ident!("{name}");
        let idx = u32::try_from(idx).unwrap();
        let vis = if is_pub {
          quote! { pub }
        } else {
          quote! {}
        };
        quote! { #vis const #name: Self = Self(#idx); }
      });
    let str_idx_debug_arms =
      identifiers.iter().chain(strings()).enumerate().map(|(idx, &(_, contents))| {
        let idx = u32::try_from(idx).unwrap();
        quote! { #idx => d.field(&#contents) }
      });
    let capacity = identifiers.len() + strings().count();
    let str_arena_inserts = identifiers.iter().chain(strings()).map(|&(name, contents)| {
      let name = format_ident!("{name}");
      quote! { assert_eq!(StrIdx::#name, ret.mk_idx(bs(#contents))); }
    });

    quote! {
      impl StrIdx {
        #(#str_idx_constants)*
      }

      impl fmt::Debug for StrIdx {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
          let mut d = f.debug_tuple("StrIdx");
          match self.to_u32() {
            #(#str_idx_debug_arms,)*
            n => d.field(&n),
          };
          d.finish()
        }
      }

      fn bs(s: &str) -> Box<str> {
        s.to_owned().into_boxed_str()
      }

      impl Default for StrArena {
        fn default() -> Self {
          let mut ret = Self {
            idx_to_contents: Vec::with_capacity(#capacity),
            contents_to_idx: FxHashMap::default(),
          };
          #(#str_arena_inserts)*
          ret
        }
      }
    }
  };

  let impl_id = {
    let constants = identifiers.iter().map(|&(name, _)| {
      let name = format_ident!("{name}");
      quote! { pub const #name: Self = Self(StrIdx::#name); }
    });

    quote! {
      impl Id {
        #(#constants)*
      }
    }
  };

  let impl_str = {
    let constants = strings().map(|&(name, _)| {
      let name = format_ident!("{name}");
      quote! { pub const #name: Self = Self(StrRepr::Idx(StrIdx::#name)); }
    });

    quote! {
      impl Str {
        #(#constants)*
      }
    }
  };

  let std_fn = {
    let variants = std_fns.iter().map(|&(name, _)| format_ident!("{name}"));
    let count = std_fns.len();
    let str_variant_tuples = std_fns.iter().map(|&(name, _)| {
      let name = format_ident!("{name}");
      quote! { (Str::#name, Self::#name) }
    });
    quote! {
      #[derive(Debug, Clone, Copy)]
      #[allow(non_camel_case_types)]
      pub enum StdFn {
        #(#variants,)*
      }

      impl StdFn {
        pub const ALL: [(Str, Self); #count] = [
          #(#str_variant_tuples,)*
        ];
      }
    }
  };

  let file = file!();

  let contents = quote! {
    pub const _GENERATED_BY: &str = #file;

    use crate::{Id, Str, StrRepr, StrIdx, StrArena};
    use rustc_hash::FxHashMap;
    use std::fmt;

    #impl_str_idx_and_arena

    #impl_id

    #impl_str

    #std_fn
  };
  write_rs_tokens::go(contents, "generated.rs");
}
