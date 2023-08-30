use crate::Parser;
use jsonnet_syntax::kind::SyntaxKind as SK;

/// returns `Some(_)` iff this consumed something because we could started parsing an expression.
#[must_use]
pub(crate) fn get(p: &mut Parser<'_>) -> Option<event_parse::Exited> {
  let Some(cur) = p.peek() else { return None };
  let en = p.enter();
  let kind = match cur.kind {
    SK::NullKw => {
      p.bump();
      SK::ExprNull
    }
    SK::TrueKw => {
      p.bump();
      SK::ExprTrue
    }
    SK::FalseKw => {
      p.bump();
      SK::ExprFalse
    }
    SK::SelfKw => {
      p.bump();
      SK::ExprSelf
    }
    // TODO forbid super in many contexts
    SK::SuperKw => {
      p.bump();
      SK::SuperKw
    }
    SK::Dollar => {
      p.bump();
      SK::ExprDollar
    }
    SK::String => {
      p.bump();
      SK::ExprString
    }
    SK::Number => {
      p.bump();
      SK::ExprNumber
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  Some(p.exit(en, kind))
}
