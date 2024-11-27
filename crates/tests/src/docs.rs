//! Tests to make sure the code examples in the docs work.

use crate::check::{markdown, Input, JsonnetInput};
use jsonnet_token::Error;

#[test]
fn tokens() {
  for tok in jsonnet_token::ALL {
    for (idx, purpose) in tok.purposes.iter().enumerate() {
      let mut ex = purpose.example.trim().to_owned();
      if ex.ends_with(';') {
        ex.push_str("null");
      }
      let jsonnet = match purpose.outcome {
        Ok(()) => JsonnetInput::manifest(&ex, ""),
        Err(Error::Eval(e)) => JsonnetInput::eval_error(&ex, e),
        Err(Error::PreEval(e)) => JsonnetInput::pre_eval_error_one(&ex, e),
        Err(Error::StackOverflow) => continue,
      };
      let path = format!("{}_{}.jsonnet", tok.text, idx);
      Input::default().with_jsonnet(path.as_str(), jsonnet).add_all().check();
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
    for (idx, &example) in f.examples.iter().enumerate() {
      let path = format!("{}_{}.jsonnet", f.name.content(), idx);
      Input::default()
        .with_jsonnet(path.as_str(), JsonnetInput::manifest(example, "true"))
        .add_all()
        .check();
    }
  }
}
