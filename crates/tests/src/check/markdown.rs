//! Checking code blocks in Markdown files.

use crate::check::JsonnetInput;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag, TagEnd};

const LANG_NAME: &str = "jsonnet";

pub(crate) fn check(contents: &str) {
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(contents, options);
  let mut inside = false;
  let mut ac = String::new();
  for ev in parser {
    match ev {
      Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(lang))) => {
        if lang.as_ref() == LANG_NAME {
          inside = true;
        }
      }
      Event::End(TagEnd::CodeBlock) => {
        if inside {
          if ac.trim_end().ends_with(';') {
            ac.push_str("null");
          }
          JsonnetInput::manifest_or_fn(ac.as_str(), "").check();
          ac.clear();
          inside = false;
        }
      }
      Event::Text(s) => {
        if inside {
          ac.push_str(s.as_ref());
        }
      }
      _ => {}
    }
  }
}
