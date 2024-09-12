//! Build some ty-related things.

use quote::{format_ident as i, quote as q};

fn main() {
  let things = [
    (i!("ANY"), q!(super::Data::Any)),
    (i!("TRUE"), q!(super::Data::True)),
    (i!("FALSE"), q!(super::Data::False)),
    (i!("NULL"), q!(super::Data::Null)),
    (i!("STRING"), q!(super::Data::String)),
    (i!("NUMBER"), q!(super::Data::Number)),
    (i!("NEVER"), q!(super::Data::Union(BTreeSet::new()))),
    (i!("BOOL"), q!(super::Data::Union(BTreeSet::from([super::Ty::TRUE, super::Ty::FALSE])))),
    (i!("ARRAY_NUMBER"), q!(super::Data::Array(super::Ty::NUMBER))),
    (i!("ARRAY_ANY"), q!(super::Data::Array(super::Ty::ANY))),
    (
      i!("ARRAY_OR_OBJECT"),
      q!(super::Data::Union(BTreeSet::from([super::Ty::ARRAY_ANY, super::Ty::OBJECT]))),
    ),
    (i!("OBJECT"), q!(super::Data::Object(super::Object::unknown()))),
  ];
  let impl_ty_const = things.iter().enumerate().map(|(idx, (name, _))| {
    #[expect(clippy::disallowed_methods, reason = "ok to panic in build script")]
    let idx = u32::try_from(idx).expect("usize to u32");
    // NOTE: we depend on the layout of Ty being just a regular index with no extra bit manipulation
    // for the shared case here.
    q! { pub(crate) const #name: Self = Self(#idx); }
  });
  let ty_data = things.iter().map(|(_, td)| td);
  let map_entries = things.iter().map(|(name, td)| q! { (#td, super::Ty::#name) });
  let file = file!();
  let all = q! {
    use std::collections::BTreeSet;

    pub const _GENERATED_BY: &str = #file;

    impl super::Ty {
      #(#impl_ty_const)*
    }

    impl super::Store {
      #[doc = "Returns a store with the builtin types, like `Ty::ANY`."]
      pub(crate) fn with_builtin() -> Self {
        Self {
          idx_to_data: vec![
            #(#ty_data,)*
          ],
          data_to_idx: rustc_hash::FxHashMap::from_iter([
            #(#map_entries,)*
          ]),
        }
      }
    }
  };
  write_rs_tokens::go(all, "generated.rs");
}
