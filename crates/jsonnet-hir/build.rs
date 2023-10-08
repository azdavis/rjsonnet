use quote::{format_ident, quote};

/// TODO replace with prettyplease from dtolnay
mod pretty {
  use std::fs::OpenOptions;
  use std::io::{Result, Write as _};
  use std::process::{Command, Stdio};

  pub(crate) fn rust(name: &std::path::Path, contents: &str) -> Result<()> {
    let prog = Command::new("rustfmt").stdin(Stdio::piped()).stdout(Stdio::piped()).spawn();
    match prog {
      Ok(mut prog) => {
        let mut stdout = prog.stdout.take().unwrap();
        let mut out_file = OpenOptions::new().write(true).create(true).truncate(true).open(name)?;
        prog.stdin.take().unwrap().write_all(contents.as_bytes())?;
        std::io::copy(&mut stdout, &mut out_file)?;
        if !prog.wait()?.success() {
          return Err(std::io::ErrorKind::Other.into());
        }
      }
      Err(_) => std::fs::write(name, contents)?,
    }
    Ok(())
  }
}

fn main() {
  let out_dir = std::env::var_os("OUT_DIR").expect("OUT_DIR should be set");
  let out_dir = std::path::Path::new(&out_dir);
  let out_path = out_dir.join("generated.rs");
  let preset = [
    ("STD", "std", true),
    ("SELF", "self", true),
    ("SUPER", "super", true),
    ("DOLLAR", "$", true),
    ("JOIN", "join", true),
    ("MAKE_ARRAY", "makeArray", true),
    ("LENGTH", "length", true),
    ("PARAMETER_NOT_BOUND", "Parameter not bound", false),
    ("TODO", "TODO", false),
  ];
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
        let mut ret = Self { id_to_contents: Vec::with_capacity(#preset_len), contents_to_id: FxHashMap::default() };
        #(#str_arena_inserts)*
        ret
      }
    }
  };
  let contents = contents.to_string();
  pretty::rust(&out_path, &contents).unwrap();
}
