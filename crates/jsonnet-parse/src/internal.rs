//! The internal impl.

use crate::{ErrorKind, Expected, Parser};
use always::always;
use event_parse::{Entered, Exited};
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
#[expect(clippy::too_many_lines)]
fn expr_prec(p: &mut Parser<'_>, min_prec: Prec) -> Option<Exited> {
  let cur = p.peek()?;
  let en = p.enter();
  let kind = match cur.kind {
    // all of these expressions are atomic. this means they never require parentheses to resolve
    // precedence issues. clearly, expressions like `null` and `false` are this way, but
    // interestingly so are object literals and imports. note that objects and arrays may contain
    // sub-expressions that are not atomic, but the overall expression is atomic.
    SK::DotDotDot => {
      p.bump();
      SK::ExprHole
    }
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
    SK::SuperKw => {
      p.bump();
      SK::ExprSuper
    }
    SK::Dollar => {
      p.bump();
      SK::ExprDollar
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
      object(p);
      SK::ExprObject
    }
    SK::LSquare => {
      p.bump();
      comma_sep(p, expr_comma, |x| matches!(x, SK::ForKw | SK::RSquare), SK::ExprComma);
      while comp_spec(p).is_some() {}
      p.eat(SK::RSquare);
      SK::ExprArray
    }
    SK::ImportKw | SK::ImportbinKw | SK::ImportstrKw => {
      p.bump();
      if string(p) {
        p.bump();
      } else {
        p.error(ErrorKind::Expected(Expected::String));
      }
      SK::ExprImport
    }
    // these expressions are not atomic, but they do begin with some single identifying token that
    // immediately distinguishes them from all other expressions, like `assert` or `function`. in
    // addition, their sub-expressions extend to the right as far as possible (as defined in the
    // spec), so we may simply recur with `expr_must` which will handle the precedence.
    SK::LocalKw => {
      p.bump();
      comma_sep(p, bind_comma, |x| x == SK::Semicolon, SK::BindComma);
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
      if string(p) {
        p.bump();
        SK::ExprString
      } else {
        p.abandon(en);
        return None;
      }
    }
  };
  // pratt parser for operator precedence
  let mut ex = p.exit(en, kind);
  while let Some(cur) = p.peek() {
    ex = match cur.kind {
      SK::LRound => {
        let en = p.precede(ex);
        p.bump();
        comma_sep(p, arg, |x| x == SK::RRound, SK::Arg);
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
      SK::LCurly => {
        let en = p.precede(ex);
        object(p);
        p.exit(en, SK::ExprImplicitObjectPlus)
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

#[must_use]
fn string(p: &mut Parser<'_>) -> bool {
  p.peek().is_some_and(|tok| {
    matches!(
      tok.kind,
      SK::DoubleQuotedString
        | SK::SingleQuotedString
        | SK::DoubleQuotedVerbatimString
        | SK::SingleQuotedVerbatimString
        | SK::TextBlock
    )
  })
}

fn object(p: &mut Parser<'_>) -> Exited {
  always!(p.at(SK::LCurly));
  let en = p.enter();
  p.bump();
  comma_sep(p, member, |x| matches!(x, SK::ForKw | SK::RCurly), SK::Member);
  while comp_spec(p).is_some() {}
  p.eat(SK::RCurly);
  p.exit(en, SK::Object)
}

#[must_use]
fn arg(p: &mut Parser<'_>) -> Option<Entered> {
  let outer = p.enter();
  if p.at(SK::Id) && p.at_n(1, SK::Eq) {
    let inner = p.enter();
    p.bump();
    p.bump();
    p.exit(inner, SK::IdEq);
    expr_must(p);
  } else if expr(p).is_none() {
    p.abandon(outer);
    return None;
  }
  Some(outer)
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

#[must_use]
fn bin_op_prec(op: SK) -> Option<Prec> {
  let ret = match op {
    SK::Star | SK::Slash | SK::Percent => Prec::Mul,
    SK::Plus | SK::Minus => Prec::Add,
    SK::LtLt | SK::GtGt => Prec::Shift,
    SK::Lt | SK::Gt | SK::LtEq | SK::GtEq | SK::InKw => Prec::Cmp,
    SK::EqEq | SK::BangEq => Prec::Eq,
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
fn member(p: &mut Parser<'_>) -> Option<Entered> {
  let ex = member_kind(p)?;
  Some(p.precede(ex))
}

#[must_use]
fn member_kind(p: &mut Parser<'_>) -> Option<Exited> {
  let cur = p.peek()?;
  let en = p.enter();
  let kind = match cur.kind {
    SK::LocalKw => {
      p.bump();
      if bind(p).is_none() {
        p.error(ErrorKind::Expected(Expected::Bind));
      }
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
    SK::LSquare => {
      p.bump();
      expr_must(p);
      p.eat(SK::RSquare);
      SK::FieldNameExpr
    }
    _ => {
      if string(p) {
        p.bump();
        SK::FieldNameString
      } else {
        p.abandon(en);
        return None;
      }
    }
  };
  Some(p.exit(en, kind))
}

#[must_use]
fn bind(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::Id) {
    return None;
  }
  let en = p.enter();
  p.bump();
  _ = paren_params(p);
  p.eat(SK::Eq);
  expr_must(p);
  Some(p.exit(en, SK::Bind))
}

#[must_use]
fn bind_comma(p: &mut Parser<'_>) -> Option<Entered> {
  let ex = bind(p)?;
  Some(p.precede(ex))
}

#[must_use]
fn paren_params(p: &mut Parser<'_>) -> Option<Exited> {
  if !p.at(SK::LRound) {
    return None;
  }
  let en = p.enter();
  p.bump();
  comma_sep(p, param, |x| x == SK::RRound, SK::Param);
  p.eat(SK::RRound);
  Some(p.exit(en, SK::ParenParams))
}

#[must_use]
fn param(p: &mut Parser<'_>) -> Option<Entered> {
  if !p.at(SK::Id) {
    return None;
  }
  let en = p.enter();
  p.bump();
  _ = eq_expr(p);
  Some(en)
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
  always!(p.at(SK::AssertKw));
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
fn expr_comma(p: &mut Parser<'_>) -> Option<Entered> {
  let ex = expr(p)?;
  Some(p.precede(ex))
}

/// `inner` must consume tokens iff it returns `Some`. returns whether `inner` consumed anything.
fn comma_sep<F, G>(p: &mut Parser<'_>, inner: F, is_end: G, wrap: SK) -> bool
where
  F: Fn(&mut Parser<'_>) -> Option<Entered>,
  G: Fn(SK) -> bool,
{
  let mut seen_any = false;
  let mut need_comma = false;
  while p.peek().is_some_and(|x| !is_end(x.kind)) {
    if need_comma {
      p.error(ErrorKind::Expected(Expected::Kind(SK::Comma)));
    }
    need_comma = true;
    let Some(en) = inner(p) else { break };
    seen_any = true;
    if p.at(SK::Comma) {
      p.bump();
      need_comma = false;
    }
    while p.at(SK::Comma) {
      p.error(ErrorKind::ExtraComma);
      p.bump();
    }
    p.exit(en, wrap);
  }
  seen_any
}
