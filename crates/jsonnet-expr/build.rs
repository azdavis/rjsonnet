//! Generate some string/identifier names.

use quote::{format_ident, quote};
use std::collections::HashSet;

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

#[allow(clippy::too_many_lines)]
fn main() {
  let identifiers = [
    S::new("std"),
    // std_unutterable is the same as std but it has a str that cannot be written in user code as an
    // id, so it will never be shadowed. it is used in desugaring.
    S::named("$std", "std_unutterable"),
    S::named("self", "self_"),
    S::named("super", "super_"),
    S::named("$", "dollar"),
  ];
  let std_fns = [
    S::new("cmp"),
    S::new("equals"),
    S::new("join"),
    S::new("length"),
    S::new("makeArray"),
    S::named("mod", "mod_"),
    S::new("objectHasEx"),
    S::new("slice"),
  ];
  let messages = [
    S::named("Assertion failed", "ASSERTION_FAILED"),
    S::named("Parameter not bound", "PARAMETER_NOT_BOUND"),
  ];
  let strings = || {
    std::iter::once(S::new("thisFile"))
      .chain(std_fns.iter().copied())
      .chain(messages.iter().copied())
  };
  let all = || identifiers.iter().copied().chain(strings());

  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for S { name, content } in all() {
    assert!(names.insert(name), "duplicate name: {name}",);
    assert!(contents.insert(content), "duplicate content: {content}",);
  }
  drop(names);
  drop(contents);

  let impl_str_idx_and_arena = {
    let str_idx_constants = std::iter::empty()
      .chain(identifiers.iter().map(|&x| (x, false)))
      .chain(strings().map(|x| (x, true)))
      .enumerate()
      .map(|(idx, (S { name, .. }, is_pub))| {
        let name = format_ident!("{name}");
        let idx = u32::try_from(idx).unwrap();
        let vis = if is_pub {
          quote! { pub }
        } else {
          quote! {}
        };
        quote! { #vis const #name: Self = Self(#idx); }
      });
    let str_idx_debug_arms = all().enumerate().map(|(idx, S { content, .. })| {
      let idx = u32::try_from(idx).unwrap();
      quote! { #idx => d.field(&#content) }
    });
    let capacity = all().count();
    let str_arena_inserts = all().map(|S { name, content }| {
      let name = format_ident!("{name}");
      quote! { assert_eq!(StrIdx::#name, ret.mk_idx(bs(#content))); }
    });

    quote! {
      #[allow(non_upper_case_globals)]
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
    let constants = identifiers.iter().map(|S { name, .. }| {
      let name = format_ident!("{name}");
      quote! { pub const #name: Self = Self(StrIdx::#name); }
    });

    quote! {
      #[allow(non_upper_case_globals)]
      impl Id {
        #(#constants)*
      }
    }
  };

  let impl_str = {
    let constants = strings().map(|S { name, .. }| {
      let name = format_ident!("{name}");
      quote! { pub const #name: Self = Self(StrRepr::Idx(StrIdx::#name)); }
    });

    quote! {
      #[allow(non_upper_case_globals)]
      impl Str {
        #(#constants)*
      }
    }
  };

  let std_fn = {
    let variants = std_fns.iter().map(|S { name, .. }| format_ident!("{name}"));
    let count = std_fns.len();
    let str_variant_tuples = std_fns.iter().map(|S { name, .. }| {
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
