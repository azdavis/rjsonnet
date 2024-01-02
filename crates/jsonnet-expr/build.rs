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
  let strings = [
    ("JOIN", "join"),
    ("MAKE_ARRAY", "makeArray"),
    ("THIS_FILE", "thisFile"),
    ("CMP", "cmp"),
    ("LENGTH", "length"),
    ("SLICE", "slice"),
    ("MOD", "mod"),
    ("EQUALS", "equals"),
    ("OBJECT_HAS_EX", "objectHasEx"),
    ("PARAMETER_NOT_BOUND", "Parameter not bound"),
    ("ASSERTION_FAILED", "Assertion failed"),
    ("TODO", "TODO"),
  ];

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for &(name, content) in identifiers.iter().chain(strings.iter()) {
    assert!(names.insert(name), "duplicate name: {name}");
    assert!(contents.insert(content), "duplicate content: {content}");
  }
  drop(names);
  drop(contents);

  let identifiers_and_strings = {
    let str_idx_constants = std::iter::empty()
      .chain(identifiers.iter().map(|x| (x, false)))
      .chain(strings.iter().map(|x| (x, true)))
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
      identifiers.iter().chain(strings.iter()).enumerate().map(|(idx, &(_, contents))| {
        let idx = u32::try_from(idx).unwrap();
        quote! { #idx => d.field(&#contents), }
      });
    let capacity = identifiers.len() + strings.len();
    let str_arena_inserts = identifiers.iter().chain(strings.iter()).map(|&(name, contents)| {
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
            #(#str_idx_debug_arms)*
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

  let identifiers_only = {
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

  let strings_only = {
    let constants = strings.iter().map(|&(name, _)| {
      let name = format_ident!("{name}");
      quote! { pub const #name: Self = Self(StrRepr::Idx(StrIdx::#name)); }
    });

    quote! {
      impl Str {
        #(#constants)*
      }
    }
  };

  let file = file!();

  let contents = quote! {
    pub const _GENERATED_BY: &str = #file;

    use crate::{Id, Str, StrRepr, StrIdx, StrArena};
    use rustc_hash::FxHashMap;
    use std::fmt;

    #identifiers_and_strings

    #identifiers_only

    #strings_only
  };
  write_rs_tokens::go(contents, "generated.rs");
}
