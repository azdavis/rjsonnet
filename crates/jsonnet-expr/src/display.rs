//! See [`expr`].

use super::{Expr, ExprArena, ExprData, StrArena};
use std::fmt;

/// Displays an expression, sort of. Mostly for debugging. (We already derive Debug.)
///
/// TODO handle precedence better, fix trailing commas
#[must_use]
pub fn expr<'a>(
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
) -> impl fmt::Display + 'a {
  DisplayExpr { e, str_ar, expr_ar, ps, relative_to }
}

#[derive(Clone, Copy)]
struct DisplayExpr<'a> {
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
}

impl<'a> DisplayExpr<'a> {
  fn with(self, e: Expr) -> DisplayExpr<'a> {
    DisplayExpr { e, ..self }
  }
}

impl<'a> fmt::Display for DisplayExpr<'a> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Some(e) = self.e else { return f.write_str("_") };
    match &self.expr_ar[e] {
      ExprData::Prim(prim) => prim.display(self.str_ar).fmt(f),
      ExprData::Object { binds, asserts, fields } => {
        f.write_str("{ ")?;
        for &(id, expr) in binds {
          write!(f, "local {} = {}, ", id.display(self.str_ar), self.with(expr))?;
        }
        for &a in asserts {
          write!(f, "assert {}, ", self.with(a))?;
        }
        for field in fields {
          let key = self.with(field.key);
          let plus = if field.plus { "+" } else { "" };
          let vis = field.vis;
          let val = self.with(field.val);
          write!(f, "{key}{plus}{vis} {val}, ")?;
        }
        f.write_str("}")
      }
      ExprData::ObjectComp { name, body, id, ary } => {
        write!(
          f,
          "{{ [{}]: {} for {} in {} }}",
          self.with(*name),
          self.with(*body),
          id.display(self.str_ar),
          self.with(*ary)
        )
      }
      ExprData::Array(elems) => {
        f.write_str("[")?;
        for &elem in elems {
          write!(f, "{}, ", self.with(elem))?;
        }
        f.write_str("]")
      }
      ExprData::Subscript { on, idx } => write!(f, "{}[{}]", self.with(*on), self.with(*idx)),
      ExprData::Call { func, positional, named } => {
        self.with(*func).fmt(f)?;
        f.write_str("(")?;
        for &arg in positional {
          write!(f, "{}, ", self.with(arg))?;
        }
        for &(name, arg) in named {
          write!(f, "{}={}, ", name.display(self.str_ar), self.with(arg))?;
        }
        f.write_str(")")
      }
      ExprData::Id(x) => x.display(self.str_ar).fmt(f),
      ExprData::Local { binds, body } => {
        f.write_str("local ")?;
        for &(bind, e) in binds {
          write!(f, "{} = {}, ", bind.display(self.str_ar), self.with(e))?;
        }
        write!(f, "; {}", self.with(*body))
      }
      ExprData::If { cond, yes, no } => {
        write!(f, "if {} then {} else {}", self.with(*cond), self.with(*yes), self.with(*no))
      }
      ExprData::BinaryOp { lhs, op, rhs } => {
        write!(f, "{} {} {}", self.with(*lhs), op, self.with(*rhs))
      }
      ExprData::UnaryOp { op, inner } => write!(f, "{}{}", op, self.with(*inner)),
      ExprData::Function { params, body } => {
        f.write_str("function(")?;
        for &(bind, default) in params {
          bind.display(self.str_ar).fmt(f)?;
          if let Some(default) = default {
            write!(f, " = {}", self.with(default))?;
          }
          f.write_str(", ")?;
        }
        f.write_str(")")?;
        self.with(*body).fmt(f)
      }
      ExprData::Error(e) => write!(f, "error {}", self.with(*e)),
      ExprData::Import { kind, path } => {
        // TODO handle escapes
        let mut p = self.ps.get_path(*path).as_path();
        if let Some(r) = self.relative_to {
          p = p.strip_prefix(r.as_path()).unwrap_or(p);
        }
        let p = p.display();
        write!(f, "{kind} \"{p}\"")
      }
    }
  }
}
