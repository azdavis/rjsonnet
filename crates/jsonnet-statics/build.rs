//! Build some ty-related things.

use quote::{format_ident as i, quote as q};

fn main() {
  let things = [
    (i!("ANY"), q!(super::Data::Any)),
    (i!("BOOL"), q!(super::Data::Bool)),
    (i!("STRING"), q!(super::Data::String)),
    (i!("NUMBER"), q!(super::Data::Number)),
    (i!("NULL"), q!(super::Data::Prim(Prim::Null))),
    (i!("TRUE"), q!(super::Data::Prim(Prim::Bool(true)))),
    (i!("FALSE"), q!(super::Data::Prim(Prim::Bool(false)))),
    (i!("NEVER"), q!(super::Data::Or(BTreeSet::new()))),
    (i!("ARRAY_NUMBER"), q!(super::Data::Array(super::Ty::NUMBER))),
  ];
  let impl_ty_const = things.iter().enumerate().map(|(idx, (name, _))| {
    // ok to panic in build script
    #[allow(clippy::disallowed_methods)]
    let idx = u32::try_from(idx).expect("usize to u32");
    q! { pub(crate) const #name: Self = Self(#idx); }
  });
  let ty_data = things.iter().map(|(_, td)| td);
  let map_entries = things.iter().map(|(name, td)| q! { (#td, super::Ty::#name) });
  let file = file!();
  let all = q! {
    use jsonnet_expr::Prim;
    use std::collections::BTreeSet;

    pub const _GENERATED_BY: &str = #file;

    impl super::Ty {
      #(#impl_ty_const)*
    }

    impl Default for super::Store {
      fn default() -> Self {
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
