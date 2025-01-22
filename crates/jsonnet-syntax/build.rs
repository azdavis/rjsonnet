//! Generate AST and CST from an ungrammar.

#![allow(clippy::disallowed_methods, reason = "can panic in build script")]

use rustc_hash::FxHashMap;

fn push_purpose(buf: &mut String, p: &jsonnet_token::TokenPurpose) {
  buf.push_str(p.doc);
  buf.push_str("\n```jsonnet\n");
  buf.push_str(p.example);
  buf.push_str("```\n");
}

fn main() {
  let doc: FxHashMap<_, _> = jsonnet_token::ALL
    .iter()
    .map(|tok| {
      let mut doc = format!("token: `{}`\n\n", tok.text);
      if tok.purposes.len() == 1 {
        let purpose = tok.purposes.first().unwrap();
        push_purpose(&mut doc, purpose);
      } else {
        doc.push_str("This token has multiple purposes:\n");
        for (idx, purpose) in tok.purposes.iter().enumerate() {
          use std::fmt::Write as _;
          write!(doc, "\n**Purpose {}**\n\n", idx + 1).unwrap();
          push_purpose(&mut doc, purpose);
        }
      }
      (tok.text, doc)
    })
    .collect();
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
      ("MergeConflictMarker", "a Git merge conflict marker"),
    ]),
    file: file!(),
  };
  syntax_gen::gen(&options);
}
