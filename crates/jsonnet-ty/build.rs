//! Build some ty-related things.

use jsonnet_std_sig::Ty;
use quote::quote as q;
use std::collections::{BTreeMap, BTreeSet};

#[expect(clippy::too_many_lines)]
fn main() {
  let things = [
    (ident("ANY"), q!(Data::Prim(super::Prim::Any))),
    (
      ident("TOP"),
      q!(Data::mk_union([
        Ty::NULL,
        Ty::TRUE,
        Ty::FALSE,
        Ty::NUMBER,
        Ty::STRING,
        Ty::ARRAY,
        Ty::OBJECT,
        Ty::FUNCTION,
      ])),
    ),
    (ident("NEVER"), q!(Data::mk_union([]))),
    (ident("NULL"), q!(Data::Prim(super::Prim::Null))),
    (ident("TRUE"), q!(Data::Prim(super::Prim::True))),
    (ident("FALSE"), q!(Data::Prim(super::Prim::False))),
    (ident("NUMBER"), q!(Data::Prim(super::Prim::Number))),
    (ident("STRING"), q!(Data::Prim(super::Prim::String))),
    (ident("ARRAY"), q!(Data::Array(super::Array::new(Ty::ANY)))),
    (ident("OBJECT"), q!(Data::Object(super::Object::unknown()))),
    (ident("FUNCTION"), q!(Data::Fn(super::Fn::Unknown))),
    (ident("BOOLEAN"), q!(Data::mk_union([Ty::TRUE, Ty::FALSE]))),
    (ident("ARRAY_BOOLEAN"), q!(Data::Array(super::Array::new(Ty::BOOLEAN)))),
    (ident("ARRAY_NUMBER"), q!(Data::Array(super::Array::new(Ty::NUMBER)))),
    (ident("ARRAY_STRING"), q!(Data::Array(super::Array::new(Ty::STRING)))),
    (ident("ARRAY_KEY_VALUE"), q!(Data::Array(super::Array::new(Ty::KEY_VALUE)))),
    (ident("SET"), q!(Data::Array(super::Array::set(Ty::ANY)))),
    (ident("ARRAY_OR_OBJECT"), q!(Data::mk_union([Ty::ARRAY, Ty::OBJECT]))),
    (ident("STRING_OR_ARRAY_NUMBER"), q!(Data::mk_union([Ty::STRING, Ty::ARRAY_NUMBER]))),
    (ident("STRING_OR_ARRAY"), q!(Data::mk_union([Ty::STRING, Ty::ARRAY]))),
    (ident("NUMBER_OR_NULL"), q!(Data::mk_union([Ty::NUMBER, Ty::NULL]))),
    (ident("NUMBER_OR_STRING"), q!(Data::mk_union([Ty::NUMBER, Ty::STRING]))),
    (ident("STD_PARAM_FN1"), q!(Data::Fn(super::Fn::StdParam(super::StdParam::One)))),
    (ident("STD_PARAM_FN_ACC_ELEM"), q!(Data::Fn(super::Fn::StdParam(super::StdParam::AccElem)))),
    (ident("STD_PARAM_FN_KEY_VALUE"), q!(Data::Fn(super::Fn::StdParam(super::StdParam::KeyValue)))),
    (ident("STD_PARAM_FN_IDX_ELEM"), q!(Data::Fn(super::Fn::StdParam(super::StdParam::IdxElem)))),
    (
      ident("KEY_VALUE"),
      q!(Data::Object(super::Object {
        known: BTreeMap::from([(Str::key, Ty::STRING), (Str::value, Ty::ANY)]),
        has_unknown: false
      })),
    ),
    (ident("STD"), q!(Data::Object(super::Object::std()))),
  ];

  let things: Vec<_> = {
    let std_fn_types = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      (name.clone(), q!(Data::Fn(super::Fn::Std(StdFn::#name))), false)
    });
    things.into_iter().map(|(a, b)| (a, b, true)).chain(std_fn_types).collect()
  };

  let impl_ty = {
    let consts = things.iter().enumerate().map(|(idx, (name, _, is_pub))| {
      #[expect(clippy::disallowed_methods, reason = "ok to panic in build script")]
      let idx = u32::try_from(idx).expect("usize to u32");
      // NOTE: we depend on the layout of Ty being just a regular index with no extra bit manipulation
      // for the shared case here.
      let vis = if *is_pub {
        q! { pub }
      } else {
        q! {}
      };
      q! { #vis const #name: Self = Self(#idx); }
    });

    let std_fn_match_arms = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      q! { StdFn::#name => Ty::#name, }
    });

    q! {
      #[expect(missing_docs, non_upper_case_globals)]
      impl Ty {
        #(#consts)*

        #[expect(clippy::too_many_lines)]
        pub(crate) fn std_fn(f: StdFn) -> Self {
          match f {
            #(#std_fn_match_arms)*
          }
        }
      }
    }
  };

  let impl_object = {
    let std_fn_map_entries = jsonnet_std_sig::FNS.iter().map(|f| {
      let name = ident(f.name.ident());
      q! { (Str::#name, Ty::#name) }
    });

    q! {
      impl super::Object {
        #[expect(clippy::too_many_lines)]
        fn std() -> Self {
          Self {
            known: BTreeMap::from([(Str::thisFile, Ty::STRING), #(#std_fn_map_entries,)*]),
            has_unknown: false,
          }
        }
      }
    }
  };

  let impl_store = {
    let ty_data = things.iter().map(|(_, td, _)| td);
    let map_entries = things.iter().map(|(name, td, _)| q! { (#td, Ty::#name) });

    q! {
      impl super::Store {
        #[doc = "Returns a store with the builtin types, like `Ty::ANY`."]
        #[expect(clippy::too_many_lines)]
        pub(crate) fn with_builtin() -> Self {
          let ret = Self {
            idx_to_data: vec![
              #(#ty_data,)*
            ],
            data_to_idx: rustc_hash::FxHashMap::from_iter([
              #(#map_entries,)*
            ]),
          };
          debug_assert_eq!(ret.data_to_idx.len(), ret.idx_to_data.len());
          ret
        }
      }
    }
  };

  let impl_std_fn_sig = {
    let all_std_sigs = {
      let mut tmp = BTreeMap::<jsonnet_std_sig::Sig, BTreeSet<jsonnet_std_sig::S>>::new();
      for f in &jsonnet_std_sig::FNS {
        assert!(tmp.entry(f.sig).or_default().insert(f.name));
      }
      tmp
    };
    let std_fn_sig_arms = all_std_sigs.iter().map(|(sig, names)| {
      let names = names.iter().map(|x| {
        let name = ident(x.ident());
        q! { StdFn::#name }
      });
      let params = sig.params.iter().map(|p| mk_param(*p));
      let ret = mk_ty(sig.ret);
      q! { #(#names)|* => StdFnSig { params: &[ #(#params,)* ], ret: #ret } }
    });

    q! {
      impl StdFnSig {
        #[doc = "Get the signature for the std fn."]
        #[must_use]
        // NOTE: we almost entirely avoid triggering the match_same_arms lint because to generate the
        // arms we iter over the unique sigs, instead of iter'ing over the std fns (some of which
        // share sigs). but despite our efforts, it is still triggered sometimes, because we have some
        // std sig Ty variants that encode extra information (like Uint) but then map to identical
        // statics tys as other ones (like Num).
        #[expect(clippy::too_many_lines, clippy::match_same_arms)]
        pub fn get(f: StdFn) -> Self {
          match f {
            #(#std_fn_sig_arms,)*
          }
        }
      }
    }
  };

  let file = file!();

  let all = q! {
    use std::collections::BTreeMap;
    use jsonnet_expr::{StdFn, Str, Id};
    use super::{Ty, Data, StdFnSig, Param};

    pub const _GENERATED_BY: &str = #file;

    #impl_ty

    #impl_object

    #impl_store

    #impl_std_fn_sig
  };
  write_rs_tokens::go(all, "generated.rs");
}

fn mk_ty(ty: Ty) -> proc_macro2::TokenStream {
  match ty {
    Ty::Any => q!(Ty::ANY),
    Ty::True => q!(Ty::TRUE),
    Ty::Bool => q!(Ty::BOOLEAN),
    Ty::Num | Ty::Uint => q!(Ty::NUMBER),
    Ty::Str | Ty::StrInterned => q!(Ty::STRING),
    Ty::ArrAny => q!(Ty::ARRAY),
    Ty::ArrBool => q!(Ty::ARRAY_BOOLEAN),
    Ty::ArrNum => q!(Ty::ARRAY_NUMBER),
    Ty::ArrStr => q!(Ty::ARRAY_STRING),
    Ty::ArrKv => q!(Ty::ARRAY_KEY_VALUE),
    Ty::SetAny => q!(Ty::SET),
    Ty::Obj => q!(Ty::OBJECT),
    Ty::StrOrArrAny => q!(Ty::STRING_OR_ARRAY),
    Ty::StrOrArrNum => q!(Ty::STRING_OR_ARRAY_NUMBER),
    Ty::NumOrNull => q!(Ty::NUMBER_OR_NULL),
    Ty::NumOrStr => q!(Ty::NUMBER_OR_STRING),
    Ty::Fn1 => q!(Ty::STD_PARAM_FN1),
    Ty::FnAccElem => q!(Ty::STD_PARAM_FN_ACC_ELEM),
    Ty::FnKv => q!(Ty::STD_PARAM_FN_KEY_VALUE),
    Ty::FnIdxElem => q!(Ty::STD_PARAM_FN_IDX_ELEM),
  }
}

fn mk_param(param: jsonnet_std_sig::Param) -> proc_macro2::TokenStream {
  let id = ident(param.name);
  let ty = mk_ty(param.ty);
  let required = param.is_required();
  q! {
    Param {
      id: Id::#id,
      ty: #ty,
      required: #required,
    }
  }
}

fn ident(s: &str) -> proc_macro2::Ident {
  quote::format_ident!("{s}")
}
