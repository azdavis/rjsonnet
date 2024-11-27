//! Tests to make sure the code examples in the docs work.

use jsonnet_token::Error;

use crate::check::{markdown, JsonnetInput};

#[test]
fn tokens() {
  for tok in jsonnet_token::ALL {
    for purpose in tok.purposes {
      let mut ex = purpose.example.trim().to_owned();
      if ex.ends_with(';') {
        ex.push_str("null");
      }
      match purpose.outcome {
        Ok(()) => JsonnetInput::manifest(&ex, "").check(),
        Err(Error::Eval(e)) => JsonnetInput::eval_error(&ex, e).check(),
        Err(Error::PreEval(e)) => JsonnetInput::pre_eval_error_one(&ex, e).check(),
        Err(Error::StackOverflow) => {}
      }
    }
  }
}

#[test]
fn std_lib() {
  for f in jsonnet_std_sig::FNS {
    if !f.implemented {
      continue;
    }
    markdown::check(f.doc);
    for example in f.examples {
      JsonnetInput::manifest(example, "true").check();
    }
  }
}
