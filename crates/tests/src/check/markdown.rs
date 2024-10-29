//! Checking code blocks in Markdown files.

use crate::check::JsonnetInput;
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag, TagEnd};

const LANG_NAME: &str = "jsonnet";

pub(crate) fn check(contents: &str) {
  let mut options = Options::empty();
  options.insert(Options::ENABLE_TABLES);
  let parser = Parser::new_ext(contents, options);
  let mut inside = false;
  let mut error_next = None::<(ErrorKind, String)>;
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
          let j = match &error_next {
            Some((k, e)) => match k {
              ErrorKind::Eval => JsonnetInput::eval_error(ac.as_str(), e.as_str()),
              ErrorKind::PreEval => JsonnetInput::pre_eval_error_one(ac.as_str(), e.as_str()),
            },
            None => JsonnetInput::manifest(ac.as_str(), ""),
          };
          j.check();
          ac.clear();
          inside = false;
          error_next = None;
        }
      }
      Event::Text(s) => {
        if inside {
          ac.push_str(s.as_ref());
        }
      }
      Event::Html(s) => {
        let s = s.trim();
        if let Some(e) = s.strip_prefix("<!-- @").and_then(|s| s.strip_suffix(" -->")) {
          let (kind, e) = e.split_once(':').expect("should have a : in the magic @ comment");
          error_next = Some((kind.parse().unwrap(), e.trim().to_owned()));
        } else if let Some(x) = s.strip_prefix("<!-- @") {
          panic!("unknown special @ comment: {x}");
        }
      }
      _ => {}
    }
  }
}

enum ErrorKind {
  Eval,
  PreEval,
}

impl std::str::FromStr for ErrorKind {
  type Err = ();

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    match s {
      "eval-error" => Ok(Self::Eval),
      "pre-eval-error" => Ok(Self::PreEval),
      _ => Err(()),
    }
  }
}
