#![deny(clippy::pedantic, missing_debug_implementations, rust_2018_idioms)]

use rustc_hash::FxHashMap;

fn main() {
  let doc =
    code_h2_md_map::get(include_str!("../../docs/tokens.md"), |tok| format!("Token: `{tok}`\n"));
  let doc: FxHashMap<_, _> = doc.iter().map(|(&k, v)| (k, v.as_str())).collect();
  let options = syntax_gen::Options {
    lang: "Jsonnet",
    trivia: &["Whitespace", "SlashSlashComment", "HashComment", "BlockComment", "Invalid"],
    grammar: include_str!("syntax.ungram"),
    doc: &doc,
    special: &FxHashMap::from_iter([
      ("Id", "an identifier"),
      ("Number", "a number"),
      ("DoubleQuotedString", "a double-quoted string"),
      ("SingleQuotedString", "a single-quoted string"),
      ("DoubleQuotedVerbatimString", "a double-quoted verbatim string"),
      ("SingleQuotedVerbatimString", "a single-quoted verbatim string"),
      ("TextBlock", "a text block"),
    ]),
    file: file!(),
  };
  syntax_gen::gen(&options);
}
