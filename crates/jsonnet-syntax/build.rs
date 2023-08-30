use rustc_hash::FxHashMap;

fn main() {
  let trivia = ["Whitespace", "SlashSlashComment", "HashComment", "BlockComment", "Invalid"];
  let grammar = include_str!("syntax.ungram");
  let doc =
    code_h2_md_map::get(include_str!("../../docs/tokens.md"), |tok| format!("Token: `{tok}`\n"));
  let doc: FxHashMap<_, _> = doc.iter().map(|(&k, v)| (k, v.as_str())).collect();
  let mut special = FxHashMap::<&str, &str>::default();
  special.insert("Id", "an identifier");
  special.insert("Number", "a number");
  special.insert("String", "a string");
  syntax_gen::gen("Jsonnet", &trivia, grammar, &doc, &special)
}
