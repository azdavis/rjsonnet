//! Generate some string/identifier names.

use quote::{format_ident, quote};
use std::collections::HashSet;

#[derive(Debug, Clone, Copy)]
struct Entry {
  name: &'static str,
  content: &'static str,
}

impl Entry {
  fn new(content: &'static str, name: Option<&'static str>) -> Entry {
    Entry { name: name.unwrap_or(content), content }
  }
}

struct Group<const N: usize>([(&'static str, Option<&'static str>); N]);

impl<const N: usize> Group<N> {
  fn iter(&self) -> impl Iterator<Item = Entry> + '_ {
    self.0.iter().map(|&(a, b)| Entry::new(a, b))
  }

  fn len(&self) -> usize {
    self.0.len()
  }
}

#[allow(clippy::too_many_lines)]
fn main() {
  let identifiers = Group([
    ("std", Some("STD")),
    // STD_UNUTTERABLE is the same as STD but it has a str that cannot be written in user code as an
    // id, so it will never be shadowed. it is used in desugaring.
    ("$std", Some("STD_UNUTTERABLE")),
    ("self", Some("SELF")),
    ("super", Some("SUPER")),
    ("$", Some("DOLLAR")),
  ]);
  let std_fns = Group([
    ("cmp", Some("CMP")),
    ("equals", Some("EQUALS")),
    ("join", Some("JOIN")),
    ("length", Some("LENGTH")),
    ("makeArray", Some("MAKE_ARRAY")),
    ("mod", Some("MOD")),
    ("objectHasEx", Some("OBJECT_HAS_EX")),
    ("slice", Some("SLICE")),
  ]);
  let messages = Group([
    ("Assertion failed", Some("ASSERTION_FAILED")),
    ("Parameter not bound", Some("PARAMETER_NOT_BOUND")),
  ]);
  let strings = || {
    std::iter::once(Entry::new("thisFile", Some("THIS_FILE")))
      .chain(std_fns.iter())
      .chain(messages.iter())
  };
  let all = || identifiers.iter().chain(strings());

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for Entry { name, content } in all() {
    assert!(names.insert(name), "duplicate name: {name}",);
    assert!(contents.insert(content), "duplicate content: {content}",);
  }
  drop(names);
  drop(contents);

  let impl_str_idx_and_arena = {
    let str_idx_constants = std::iter::empty()
      .chain(identifiers.iter().map(|x| (x, false)))
      .chain(strings().map(|x| (x, true)))
      .enumerate()
      .map(|(idx, (Entry { name, .. }, is_pub))| {
        let name = format_ident!("{name}");
        let idx = u32::try_from(idx).unwrap();
        let vis = if is_pub {
          quote! { pub }
        } else {
          quote! {}
        };
        quote! { #vis const #name: Self = Self(#idx); }
      });
    let str_idx_debug_arms = all().enumerate().map(|(idx, Entry { content, .. })| {
      let idx = u32::try_from(idx).unwrap();
      quote! { #idx => d.field(&#content) }
    });
    let capacity = all().count();
    let str_arena_inserts = all().map(|Entry { name, content }| {
      let name = format_ident!("{name}");
      quote! { assert_eq!(StrIdx::#name, ret.mk_idx(bs(#content))); }
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
    let constants = identifiers.iter().map(|Entry { name, .. }| {
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
    let constants = strings().map(|Entry { name, .. }| {
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
    let variants = std_fns.iter().map(|Entry { name, .. }| format_ident!("{name}"));
    let count = std_fns.len();
    let str_variant_tuples = std_fns.iter().map(|Entry { name, .. }| {
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
