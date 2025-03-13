//! Displaying various things.

use super::{BinOp, Expr, ExprArena, ExprData, ImportKind, Prim, StrArena, UnOp, Vis};
use std::fmt;

/// Displays an expression, sort of. Mostly for debugging. (We already derive Debug.)
///
/// Doesn't have great handling for precedence or trailing commas
#[must_use]
pub fn expr<'a>(
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
) -> impl fmt::Display {
  ExprDisplay { e, str_ar, expr_ar, ps, relative_to }
}

#[derive(Clone, Copy)]
struct ExprDisplay<'a> {
  e: Expr,
  str_ar: &'a StrArena,
  expr_ar: &'a ExprArena,
  ps: &'a paths::Store,
  relative_to: Option<&'a paths::CleanPath>,
}

impl<'a> ExprDisplay<'a> {
  fn with(self, e: Expr) -> ExprDisplay<'a> {
    ExprDisplay { e, ..self }
  }
}

impl fmt::Display for ExprDisplay<'_> {
  #[expect(clippy::too_many_lines)]
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let Some(e) = self.e else { return f.write_str("_") };
    match &self.expr_ar[e] {
      ExprData::Prim(prim) => prim.display(self.str_ar).fmt(f),
      ExprData::Object { asserts, fields } => {
        f.write_str("{ ")?;
        for &a in asserts {
          write!(f, "assert {}, ", self.with(a))?;
        }
        for field in fields {
          let key = self.with(field.key);
          let vis = field.vis;
          let val = self.with(field.val);
          write!(f, "{key}{vis} {val}, ")?;
        }
        f.write_str("}")
      }
      ExprData::ObjectComp { name, vis, body, id, ary } => {
        write!(
          f,
          "{{ [{}]{vis} {} for {} in {} }}",
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
      ExprData::Subscript { on, idx } => {
        let idx_s = idx.and_then(|x| {
          if let ExprData::Prim(Prim::String(s)) = self.expr_ar[x] {
            let s = self.str_ar.get(s);
            jsonnet_ident::is(s.as_bytes()).then_some(s)
          } else {
            None
          }
        });
        if let Some(idx_s) = idx_s {
          write!(f, "{}.{}", self.with(*on), idx_s)
        } else {
          write!(f, "{}[{}]", self.with(*on), self.with(*idx))
        }
      }
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
      ExprData::BinOp { lhs, op, rhs } => {
        write!(f, "{} {} {}", self.with(*lhs), op, self.with(*rhs))
      }
      ExprData::UnOp { op, inner } => write!(f, "{}{}", op, self.with(*inner)),
      ExprData::Fn { params, body } => {
        f.write_str("function(")?;
        for &(bind, default) in params {
          bind.display(self.str_ar).fmt(f)?;
          if let Some(default) = default {
            write!(f, " = {}", self.with(default))?;
          }
          f.write_str(", ")?;
        }
        f.write_str(") ")?;
        self.with(*body).fmt(f)
      }
      ExprData::Error(e) => write!(f, "error {}", self.with(*e)),
      ExprData::Import { kind, path } => {
        let mut p = self.ps.get_path(*path).as_path();
        if let Some(r) = self.relative_to {
          p = p.strip_prefix(r.as_path()).unwrap_or(p);
        }
        let p = p.as_os_str().as_encoded_bytes();
        let p = jsonnet_escape::Unescape::new(p);
        write!(f, "{kind} {p}")
      }
      ExprData::SubstOuter(e) => {
        let e = self.with(*e);
        write!(f, "({e})[$outerself/self,$outersuper/super]")
      }
    }
  }
}

impl fmt::Display for ImportKind {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      ImportKind::Code => "import",
      ImportKind::String => "importstr",
      ImportKind::Binary => "importbin",
    };
    f.write_str(s)
  }
}

impl fmt::Display for Vis {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      Vis::Default => ":",
      Vis::Hidden => "::",
      Vis::Visible => ":::",
    };
    f.write_str(s)
  }
}

impl fmt::Display for BinOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      BinOp::Mul => "*",
      BinOp::Div => "/",
      BinOp::Add => "+",
      BinOp::Sub => "-",
      BinOp::Shl => "<<",
      BinOp::Shr => ">>",
      BinOp::Lt => "<",
      BinOp::LtEq => "<=",
      BinOp::Eq => "==",
      BinOp::Gt => ">",
      BinOp::GtEq => ">=",
      BinOp::BitAnd => "&",
      BinOp::BitXor => "^",
      BinOp::BitOr => "|",
    };
    f.write_str(s)
  }
}

impl fmt::Display for UnOp {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let s = match self {
      UnOp::Neg => "-",
      UnOp::Pos => "+",
      UnOp::LogicalNot => "!",
      UnOp::BitNot => "~",
    };
    f.write_str(s)
  }
}

impl Prim {
  #[must_use]
  pub fn display<'a>(&'a self, ar: &'a StrArena) -> impl fmt::Display {
    PrimDisplay { prim: self, ar }
  }
}

struct PrimDisplay<'a> {
  prim: &'a Prim,
  ar: &'a StrArena,
}

impl fmt::Display for PrimDisplay<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self.prim {
      Prim::Null => f.write_str("null"),
      Prim::Bool(b) => b.fmt(f),
      Prim::String(s) => {
        let bs = self.ar.get(*s).as_bytes();
        jsonnet_escape::Unescape::new(bs).fmt(f)
      }
      Prim::Number(n) => n.fmt(f),
    }
  }
}

impl fmt::Display for crate::StdFn {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str(self.as_static_str())
  }
}
