use rustc_hash::FxHashMap;

fn main() {
  let trivia = ["Whitespace", "SlashSlashComment", "HashComment", "BlockComment", "Invalid"];
  let grammar = include_str!("syntax.ungram");
  let doc =
    code_h2_md_map::get(include_str!("../../docs/tokens.md"), |tok| format!("Token: `{tok}`\n"));
  let doc: FxHashMap<_, _> = doc.iter().map(|(&k, v)| (k, v.as_str())).collect();
  let special = FxHashMap::from_iter([
    ("Id", "an identifier"),
    ("Number", "a number"),
    ("DoubleQuotedString", "a double-quoted string"),
    ("SingleQuotedString", "a single-quoted string"),
    ("DoubleQuotedVerbatimString", "a double-quoted verbatim string"),
    ("SingleQuotedVerbatimString", "a single-quoted verbatim string"),
    ("TextBlock", "a text block"),
  ]);
  syntax_gen::gen("Jsonnet", &trivia, grammar, &doc, &special)
}
