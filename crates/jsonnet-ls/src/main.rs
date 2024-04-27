//! A language server for Jsonnet.

fn main() {
  lang_srv::run::<jsonnet_analyze::St>();
}
