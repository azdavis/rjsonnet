//! Generate AST and CST from an ungrammar.

use rustc_hash::FxHashMap;

fn main() {
  let doc =
    code_h2_md_map::get(include_str!("../../docs/tokens.md"), |tok| format!("token: `{tok}`\n"));
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
