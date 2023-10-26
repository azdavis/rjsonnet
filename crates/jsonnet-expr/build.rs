use quote::{format_ident, quote};
use std::collections::HashSet;

fn main() {
  let preset = [
    ("STD", "std", true),
    // STD_UNUTTERABLE is the same as STD but it has a str that cannot be written in user code as an
    // id, so it will never be shadowed. it is used in desugaring.
    ("STD_UNUTTERABLE", "$std", true),
    ("SELF", "self", true),
    ("SUPER", "super", true),
    // OUTER_SELF and OUTER_SUPER are also unutterable and are used in the desugaring.
    ("OUTER_SELF", "$outerself", true),
    ("OUTER_SUPER", "$outersuper", true),
    ("DOLLAR", "$", true),
    ("JOIN", "join", true),
    ("MAKE_ARRAY", "makeArray", true),
    ("LENGTH", "length", true),
    ("SLICE", "slice", true),
    ("MOD", "mod", true),
    ("EQUALS", "equals", true),
    ("OBJECT_HAS_EX", "objectHasEx", true),
    ("PARAMETER_NOT_BOUND", "Parameter not bound", false),
    ("ASSERTION_FAILED", "Assertion failed", false),
    ("TODO", "TODO", false),
  ];
  let mut names = HashSet::<&'static str>::new();
  let mut contents = HashSet::<&'static str>::new();
  for &(name, content, _) in &preset {
    assert!(names.insert(name), "duplicate name: {name}");
    assert!(contents.insert(content), "duplicate content: {content}");
  }
  let str_constants = preset.iter().enumerate().map(|(idx, &(name, _, make_id))| {
    let name = format_ident!("{name}");
    let idx = u32::try_from(idx).unwrap();
    let vis = if make_id {
      quote! {}
    } else {
      quote! { pub }
    };
    quote! { #vis const #name: Self = Self(#idx); }
  });
  let id_constants = preset.iter().filter_map(|&(name, _, make_id)| {
    if !make_id {
      return None;
    }
    let name = format_ident!("{name}");
    Some(quote! { pub const #name: Self = Self(Str::#name); })
  });
  let str_arena_inserts = preset.iter().map(|&(name, contents, _)| {
    let name = format_ident!("{name}");
    quote! { assert_eq!(Str::#name, ret.insert(#contents.to_owned().into_boxed_str())); }
  });
  let preset_len = preset.len();
  let contents = quote! {
    use crate::{Id, Str, StrArena};
    use rustc_hash::FxHashMap;

    impl Str {
      #(#str_constants)*
    }

    impl Id {
      #(#id_constants)*
    }

    impl Default for StrArena {
      fn default() -> Self {
        let mut ret = Self {
          id_to_contents: Vec::with_capacity(#preset_len),
          contents_to_id: FxHashMap::default(),
        };
        #(#str_arena_inserts)*
        ret
      }
    }
  };
  let file = syn::parse2(contents).unwrap();
  let formatted = prettyplease::unparse(&file);
  let out_dir = std::env::var_os("OUT_DIR").unwrap();
  let dst = std::path::Path::new(&out_dir).join("generated.rs");
  std::fs::write(dst, formatted).unwrap();
}
