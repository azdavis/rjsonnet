//! Tests for the sigs.

use rustc_hash::FxHashSet;

#[test]
fn ok() {
  let mut ident = FxHashSet::<&str>::default();
  let mut content = FxHashSet::<&str>::default();

  for f in super::FNS {
    assert!(ident.insert(f.name.ident()), "duplicate ident: {}", f.name.ident());
    assert!(content.insert(f.name.content()), "duplicate content: {}", f.name.content());

    let has_enough = !f.examples.is_empty();
    let is_exempt = matches!(
      f.name.content(),
      "extVar"
        | "mod"
        | "manifestIni"
        | "manifestPython"
        | "manifestPythonVars"
        | "manifestJsonEx"
        | "manifestJson"
        | "manifestJsonMinified"
        | "manifestYamlDoc"
        | "manifestYamlStream"
        | "manifestXmlJsonml"
        | "manifestTomlEx"
        | "sort"
        | "base64Decode"
        | "trace"
    );
    let has_enough_when_not_exempt = has_enough != is_exempt;
    assert!(
      has_enough_when_not_exempt,
      "either have examples and not exempt, or have no examples and exempt: `{}`",
      f.name.content(),
    );

    for example in f.examples {
      let want = format!("std.{}(", f.name.content());
      let ok = example.contains(&want) || (f.name.content() == "format" && example.contains('%'));
      assert!(
        ok,
        "example for `{}` doesn't test the function: {}",
        f.name.content(),
        example.trim(),
      );
    }
  }
}
