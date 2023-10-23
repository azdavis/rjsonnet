//! The internal impl.
//!
//! TODO forbid no comma between members, expr commas, params, etc. maybe do it in lowering?

use crate::{ErrorKind, Expected, Parser};
use event_parse::Exited;
use jsonnet_syntax::kind::SyntaxKind as SK;

/// errors but does not advance iff no expr
pub(crate) fn expr_must(p: &mut Parser<'_>) {
  expr_prec_must(p, Prec::Min);
}

/// returns `Some(_)` iff this consumed something because we could started parsing an expression.
#[must_use]
fn expr(p: &mut Parser<'_>) -> Option<Exited> {
  expr_prec(p, Prec::Min)
}

fn expr_prec_must(p: &mut Parser<'_>, min_prec: Prec) {
  if expr_prec(p, min_prec).is_none() {
    p.error(ErrorKind::Expected(Expected::Expr));
  }
}

/// handles precedence.
#[must_use]
#[allow(clippy::too_many_lines)]
fn expr_prec(p: &mut Parser<'_>, min_prec: Prec) -> Option<Exited> {
  let cur = p.peek()?;
  let en = p.enter();
  let kind = match cur.kind {
    // all of these expressions are atomic. this means they never require parentheses to resolve
    // precedence issues. clearly, expressions like `null` and `false` are this way, but
    // interestingly so are object literals and imports. note that objects and arrays may contain
    // sub-expressions that are not atomic, but the overall expression is atomic.
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
      SK::ExprSuper
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
    SK::Id => {
      p.bump();
      SK::ExprId
    }
    SK::LRound => {
      p.bump();
      expr_must(p);
      p.eat(SK::RRound);
      SK::ExprParen
    }
    SK::LCurly => {
      p.bump();
      while member(p).is_some() {}
      while comp_spec(p).is_some() {}
      p.eat(SK::RCurly);
      SK::ExprObject
    }
    SK::LSquare => {
      p.bump();
      while expr_comma(p).is_some() {}
      while comp_spec(p).is_some() {}
      p.eat(SK::RSquare);
      SK::ExprArray
    }
    SK::ImportKw | SK::ImportbinKw | SK::ImportstrKw => {
      p.bump();
      p.eat(SK::String);
      SK::ExprImport
    }
    // these expressions are not atomic, but they do begin with some single identifying token that
    // immediately distinguishes them from all other expressions, like `assert` or `function`. in
    // addition, their sub-expressions extend to the right as far as possible (as defined in the
    // spec), so we may simply recur with `expr_must` which will handle the precedence.
    SK::LocalKw => {
      p.bump();
      bind(p);
      p.eat(SK::Semicolon);
      expr_must(p);
      SK::ExprLocal
    }
    SK::IfKw => {
      p.bump();
      expr_must(p);
      p.eat(SK::ThenKw);
      expr_must(p);
      if p.at(SK::ElseKw) {
        let en = p.enter();
        p.bump();
        expr_must(p);
        p.exit(en, SK::ElseExpr);
      }
      SK::ExprIf
    }
    SK::FunctionKw => {
      p.bump();
      if paren_params(p).is_none() {
        p.error(ErrorKind::Expected(Expected::Kind(SK::LRound)));
      }
      expr_must(p);
      SK::ExprFunction
    }
    SK::AssertKw => {
      let en = p.enter();
      let k = assert_(p);
      p.exit(en, k);
      p.eat(SK::Semicolon);
      expr_must(p);
      SK::ExprAssert
    }
    SK::ErrorKw => {
      p.bump();
      expr_must(p);
      SK::ExprError
    }
    // the unary operator expressions are not atomic, and they do begin with an identifying token,
    // but we cannot recur with simply `expr_must`, because we must care a bit more about precedence
    // here.
    SK::Minus | SK::Plus | SK::Bang | SK::Tilde => {
      p.bump();
      expr_prec_must(p, Prec::Unary);
      SK::ExprUnaryOp
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  // pratt parser for operator precedence
  let mut ex = p.exit(en, kind);
  while let Some(cur) = p.peek() {
    ex = match cur.kind {
      SK::LRound => {
        let en = p.precede(ex);
        p.bump();
        while arg(p).is_some() {}
        p.eat(SK::RRound);
        p.exit(en, SK::ExprCall)
      }
      SK::LSquare => {
        let en = p.precede(ex);
        p.bump();
        _ = expr(p);
        if p.at(SK::Colon) {
          p.bump();
        }
        _ = expr(p);
        if p.at(SK::Colon) {
          p.bump();
        }
        _ = expr(p);
        p.eat(SK::RSquare);
        p.exit(en, SK::ExprSubscript)
      }
      SK::Dot => {
        let en = p.precede(ex);
        p.bump();
        p.eat(SK::Id);
        p.exit(en, SK::ExprFieldGet)
      }
      SK::TailstrictKw => {
        let en = p.precede(ex);
        p.bump();
        p.exit(en, SK::ExprTailstrict)
      }
      op => match bin_op_prec(op) {
        Some(op_prec) => {
          if op_prec <= min_prec {
            break;
          }
          let en = p.precede(ex);
          p.bump();
          expr_prec_must(p, op_prec);
          p.exit(en, SK::ExprBinaryOp)
        }
        None => break,
      },
    };
  }
  Some(ex)
}

fn arg(p: &mut Parser<'_>) -> Option<Exited> {
  let outer = p.enter();
  if p.at(SK::Id) {
    let inner = p.enter();
    p.bump();
    if p.at(SK::Eq) {
      p.bump();
      p.exit(inner, SK::IdEq);
      expr_must(p);
    } else {
      p.exit(inner, SK::ExprId);
    }
  } else if expr(p).is_none() {
    p.abandon(outer);
    return None;
  }
  if p.at(SK::Comma) {
    p.bump();
  }
  Some(p.exit(outer, SK::Arg))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Prec {
  Min,
  LogicalOr,
  LogicalAnd,
  BitOr,
  BitXor,
  BitAnd,
  Eq,
  Cmp,
  Shift,
  Add,
  Mul,
  Unary,
}

fn bin_op_prec(op: SK) -> Option<Prec> {
  let ret = match op {
    SK::Star | SK::Slash | SK::Percent => Prec::Mul,
    SK::Plus | SK::Minus => Prec::Add,
    SK::LtLt | SK::GtGt => Prec::Shift,
    SK::Lt | SK::Gt | SK::LtEq | SK::GtEq | SK::InKw => Prec::Cmp,
    SK::Eq | SK::BangEq => Prec::Eq,
    SK::And => Prec::BitAnd,
    SK::Carat => Prec::BitXor,
    SK::Bar => Prec::BitOr,
    SK::AndAnd => Prec::LogicalAnd,
    SK::BarBar => Prec::LogicalOr,
    _ => return None,
  };
  Some(ret)
}

#[must_use]
fn comp_spec(p: &mut Parser<'_>) -> Option<Exited> {
  let cur = p.peek()?;
  match cur.kind {
    SK::ForKw => {
      let en = p.enter();
      p.bump();
      p.eat(SK::Id);
      p.eat(SK::InKw);
      expr_must(p);
      Some(p.exit(en, SK::ForSpec))
    }
    SK::IfKw => {
      let en = p.enter();
      p.bump();
      expr_must(p);
      Some(p.exit(en, SK::IfSpec))
    }
    _ => None,
  }
}

#[must_use]
fn member(p: &mut Parser<'_>) -> Option<Exited> {
  let ex = member_kind(p)?;
  let en = p.precede(ex);
  if p.at(SK::Comma) {
    p.bump();
  }
  Some(p.exit(en, SK::Member))
}

#[must_use]
fn member_kind(p: &mut Parser<'_>) -> Option<Exited> {
  let cur = p.peek()?;
  let en = p.enter();
  let kind = match cur.kind {
    SK::LocalKw => {
      p.bump();
      bind(p);
      SK::ObjectLocal
    }
    SK::AssertKw => assert_(p),
    _ => {
      if field(p) {
        SK::Field
      } else {
        p.abandon(en);
        return None;
      }
    }
  };
  Some(p.exit(en, kind))
}

#[must_use]
fn field(p: &mut Parser<'_>) -> bool {
  if field_name(p).is_none() {
    return false;
  }
  if p.at(SK::Plus) {
    let en = p.enter();
    p.bump();
    p.exit(en, SK::FieldPlus);
  } else {
    _ = paren_params(p);
  }
  let got_visibility = p
    .peek()
    .is_some_and(|cur| matches!(cur.kind, SK::Colon | SK::ColonColon | SK::ColonColonColon));
  if got_visibility {
    p.bump();
  } else {
    p.error(ErrorKind::Expected(Expected::Visibility));
  }
  expr_must(p);
  true
}

#[must_use]
fn field_name(p: &mut Parser<'_>) -> Option<Exited> {
  let cur = p.peek()?;
  let en = p.enter();
  let kind = match cur.kind {
    SK::Id => {
      p.bump();
      SK::FieldNameId
    }
    SK::String => {
      p.bump();
      SK::FieldNameString
    }
    SK::LSquare => {
      p.bump();
      expr_must(p);
      p.eat(SK::RSquare);
      SK::FieldNameExpr
    }
    _ => {
      p.abandon(en);
      return None;
    }
  };
  Some(p.exit(en, kind))
}

fn bind(p: &mut Parser<'_>) -> Exited {
  let en = p.enter();
  p.eat(SK::Id);
  _ = paren_params(p);
  p.eat(SK::Eq);
  expr_must(p);
  if p.at(SK::Comma) {
    p.bump();
  }
  p.exit(en, SK::Bind)
}

#[must_use]
fn paren_params(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::LRound) {
    return None;
  }
  let en = p.enter();
  p.bump();
  while param(p).is_some() {}
  p.eat(SK::RRound);
  Some(p.exit(en, SK::ParenParams))
}

#[must_use]
fn param(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::Id) {
    return None;
  }
  let en = p.enter();
  p.bump();
  _ = eq_expr(p);
  if p.at(SK::Comma) {
    p.bump();
  }
  Some(p.exit(en, SK::Param))
}

#[must_use]
fn eq_expr(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::Eq) {
    return None;
  }
  let en = p.enter();
  p.bump();
  expr_must(p);
  Some(p.exit(en, SK::EqExpr))
}

/// requires we are at `AssertKw`. does NOT produce an `Exited`.
#[must_use]
fn assert_(p: &mut Parser<'_>) -> SK {
  assert!(p.at(SK::AssertKw));
  p.bump();
  expr_must(p);
  if p.at(SK::Colon) {
    let en = p.enter();
    p.bump();
    expr_must(p);
    p.exit(en, SK::ColonExpr);
  }
  SK::Assert
}

#[must_use]
fn expr_comma(p: &mut Parser<'_>) -> Option<Exited> {
  let ex = expr(p)?;
  let en = p.precede(ex);
  if p.at(SK::Comma) {
    p.bump();
  }
  Some(p.exit(en, SK::ExprComma))
}
