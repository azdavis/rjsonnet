use rustc_hash::FxHashMap;

fn main() {
  let trivia = ["SlashSlashComment", "HashComment", "BlockComment"];
  let grammar = include_str!("syntax.ungram");
  let mut special = FxHashMap::<&str, &str>::default();
  special.insert("Id", "an identifier");
  special.insert("Number", "a number");
  special.insert("String", "a string");
  syntax_gen::gen("Jsonnet", &trivia, grammar, &FxHashMap::default(), &special)
}
