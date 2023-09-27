//! The internal impl.

use crate::st::St;
use jsonnet_hir::{ExprData, Id, Prim, Str};
use jsonnet_syntax::{ast, kind::SyntaxToken};

pub(crate) fn root(st: &mut St, r: ast::Root) -> jsonnet_hir::Expr {
  expr(st, r.expr(), false)
}

fn expr(st: &mut St, e: Option<ast::Expr>, in_obj: bool) -> jsonnet_hir::Expr {
  let data = match e? {
    ast::Expr::ExprNull(_) => ExprData::Prim(Prim::Null),
    ast::Expr::ExprTrue(_) => ExprData::Prim(Prim::Bool(true)),
    ast::Expr::ExprFalse(_) => ExprData::Prim(Prim::Bool(false)),
    ast::Expr::ExprSelf(_) => ExprData::Id(Id::SELF),
    ast::Expr::ExprSuper(_) => ExprData::Id(Id::SUPER),
    ast::Expr::ExprDollar(_) => ExprData::Id(Id::DOLLAR),
    ast::Expr::ExprString(_) => ExprData::Prim(Prim::String(Str::TODO)),
    // TODO
    ast::Expr::ExprNumber(_) => ExprData::Prim(Prim::Number(0.0)),
    ast::Expr::ExprId(e) => ExprData::Id(id(st, e.id()?)),
    ast::Expr::ExprParen(e) => return expr(st, e.expr(), in_obj),
    ast::Expr::ExprObject(_) => todo!(),
    ast::Expr::ExprArray(_) => todo!(),
    ast::Expr::ExprFieldGet(_) => todo!(),
    ast::Expr::ExprSubscript(_) => todo!(),
    ast::Expr::ExprCall(_) => todo!(),
    ast::Expr::ExprLocal(_) => todo!(),
    ast::Expr::ExprIf(_) => todo!(),
    ast::Expr::ExprBinaryOp(_) => todo!(),
    ast::Expr::ExprUnaryOp(_) => todo!(),
    ast::Expr::ExprImplicitObjectPlus(_) => todo!(),
    ast::Expr::ExprFunction(_) => todo!(),
    ast::Expr::ExprAssert(_) => todo!(),
    ast::Expr::ExprImport(_) => todo!(),
    ast::Expr::ExprError(_) => todo!(),
  };
  Some(st.expr(data))
}

fn id(st: &mut St, id: SyntaxToken) -> Id {
  Id::new(st.str(id.text()))
}
