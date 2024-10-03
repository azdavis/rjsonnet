//! Build some ty-related things.

use quote::{format_ident as i, quote as q};

#[expect(clippy::too_many_lines)]
fn main() {
  let things = [
    (i!("ANY"), q!(Data::Prim(super::Prim::Any))),
    (i!("TRUE"), q!(Data::Prim(super::Prim::True))),
    (i!("FALSE"), q!(Data::Prim(super::Prim::False))),
    (i!("NULL"), q!(Data::Prim(super::Prim::Null))),
    (i!("STRING"), q!(Data::Prim(super::Prim::String))),
    (i!("NUMBER"), q!(Data::Prim(super::Prim::Number))),
    (i!("NEVER"), q!(Data::Union(BTreeSet::new()))),
    (i!("BOOL"), q!(Data::Union(BTreeSet::from([Ty::TRUE, Ty::FALSE])))),
    (i!("ARRAY_NUMBER"), q!(Data::Array(Ty::NUMBER))),
    (i!("ARRAY_ANY"), q!(Data::Array(Ty::ANY))),
    (i!("ARRAY_OR_OBJECT"), q!(Data::Union(BTreeSet::from([Ty::ARRAY_ANY, Ty::OBJECT])))),
    (i!("OBJECT"), q!(Data::Object(super::Object::unknown()))),
    (i!("STD"), q!(Data::Object(super::Object::std()))),
  ];
  let std_fn_types = jsonnet_std::FNS.iter().map(|f| {
    let name = i!("{}", f.name.ident());
    (name.clone(), q!(Data::Fn(super::Fn::Std(StdFn::#name))), false)
  });
  let std_fn_match_arms = jsonnet_std::FNS.iter().map(|f| {
    let name = i!("{}", f.name.ident());
    q! { StdFn::#name => Ty::#name, }
  });
  let std_map_entries = jsonnet_std::FNS.iter().map(|f| {
    let name = i!("{}", f.name.ident());
    q! { (Str::#name, Ty::#name) }
  });
  let std_fn_sig_arms = jsonnet_std::FNS.iter().map(|f| {
    let name = i!("{}", f.name.ident());
    let body = match f.sig {
      jsonnet_std::Sig::Simple(params, ret) => {
        let params = params.iter().map(|p| mk_param(*p));
        let ret = mk_ty(ret);
        q! { StdFnSig::Simple(&[ #(#params,)* ], #ret) }
      }
      jsonnet_std::Sig::Complex(_) => q! { StdFnSig::Complex(ComplexStdFn::#name) },
    };
    q! { StdFn::#name => #body }
  });
  let complex_std_fn_variants = jsonnet_std::FNS.iter().filter_map(|f| {
    let jsonnet_std::Sig::Complex(..) = f.sig else { return None };
    let name = i!("{}", f.name.ident());
    Some(q! { #name })
  });
  let things: Vec<_> = things.into_iter().map(|(a, b)| (a, b, true)).chain(std_fn_types).collect();
  let impl_ty_const = things.iter().enumerate().map(|(idx, (name, _, pub_crate))| {
    #[expect(clippy::disallowed_methods, reason = "ok to panic in build script")]
    let idx = u32::try_from(idx).expect("usize to u32");
    // NOTE: we depend on the layout of Ty being just a regular index with no extra bit manipulation
    // for the shared case here.
    let vis = if *pub_crate {
      q! { pub(crate) }
    } else {
      q! {}
    };
    q! { #vis const #name: Self = Self(#idx); }
  });
  let ty_data = things.iter().map(|(_, td, _)| td);
  let map_entries = things.iter().map(|(name, td, _)| q! { (#td, Ty::#name) });
  let file = file!();
  let all = q! {
    use std::collections::{BTreeMap, BTreeSet};
    use jsonnet_expr::{StdFn, Str};
    use super::{Ty, Data, StdFnSig};

    pub const _GENERATED_BY: &str = #file;

    #[expect(non_upper_case_globals)]
    impl Ty {
      #(#impl_ty_const)*

      #[expect(clippy::too_many_lines)]
      pub(crate) fn std_fn(f: StdFn) -> Self {
        match f {
          #(#std_fn_match_arms)*
        }
      }
    }

    impl super::Object {
      #[expect(clippy::too_many_lines)]
      fn std() -> Self {
        Self {
          known: BTreeMap::from([(Str::thisFile, Ty::STRING), #(#std_map_entries,)*]),
          has_unknown: false,
        }
      }
    }

    impl super::Store {
      #[doc = "Returns a store with the builtin types, like `Ty::ANY`."]
      #[expect(clippy::too_many_lines)]
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

    #[derive(Debug, Clone, Copy)]
    #[expect(non_camel_case_types)]
    pub(crate) enum ComplexStdFn {
      #(#complex_std_fn_variants,)*
    }

    impl StdFnSig {
      #[expect(clippy::too_many_lines, clippy::match_same_arms)]
      pub(crate) fn get(f: StdFn) -> Self {
        match f {
          #(#std_fn_sig_arms,)*
        }
      }
    }
  };
  write_rs_tokens::go(all, "generated.rs");
}

fn mk_ty(ty: jsonnet_std::Ty) -> proc_macro2::TokenStream {
  match ty {
    jsonnet_std::Ty::Any => q!(crate::ty::Ty::ANY),
    jsonnet_std::Ty::Bool => q!(crate::ty::Ty::BOOL),
    jsonnet_std::Ty::Num => q!(crate::ty::Ty::NUMBER),
    jsonnet_std::Ty::Str => q!(crate::ty::Ty::STRING),
  }
}

fn mk_param(param: jsonnet_std::Param) -> proc_macro2::TokenStream {
  let id = i!("{}", param.name);
  let ty = mk_ty(param.ty);
  let required = param.required;
  q! {
    crate::ty::Param {
      id: jsonnet_expr::Id::#id,
      ty: #ty,
      required: #required,
    }
  }
}
