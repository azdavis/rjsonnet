//! The internal impl.

use crate::st::St;
use jsonnet_hir::{ExprData, Id, Prim, Str};
use jsonnet_syntax::{ast, kind::SyntaxToken};

pub(crate) fn get_root(st: &mut St, r: ast::Root) -> jsonnet_hir::Expr {
  get_expr(st, r.expr(), false)
}

fn get_expr(st: &mut St, e: Option<ast::Expr>, in_obj: bool) -> jsonnet_hir::Expr {
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
    ast::Expr::ExprId(e) => ExprData::Id(get_id(st, e.id()?)),
    ast::Expr::ExprParen(e) => return get_expr(st, e.expr(), in_obj),
    ast::Expr::ExprObject(_) => todo!(),
    ast::Expr::ExprArray(_) => todo!(),
    ast::Expr::ExprFieldGet(_) => todo!(),
    ast::Expr::ExprSubscript(_) => todo!(),
    ast::Expr::ExprCall(_) => todo!(),
    ast::Expr::ExprLocal(e) => {
      let mut binds = Vec::<(Id, jsonnet_hir::Expr)>::new();
      for bind in e.binds() {
        let lhs = get_id(st, bind.id()?);
        let rhs = bind.expr();
        let rhs = match bind.paren_params() {
          None => get_expr(st, rhs, in_obj),
          Some(params) => get_fn(st, params, rhs, in_obj),
        };
        binds.push((lhs, rhs));
      }
      let body = get_expr(st, e.expr(), in_obj);
      ExprData::Local { binds, body }
    }
    ast::Expr::ExprIf(_) => todo!(),
    ast::Expr::ExprBinaryOp(_) => todo!(),
    ast::Expr::ExprUnaryOp(_) => todo!(),
    ast::Expr::ExprImplicitObjectPlus(_) => todo!(),
    ast::Expr::ExprFunction(e) => return get_fn(st, e.paren_params()?, e.expr(), in_obj),
    ast::Expr::ExprAssert(_) => todo!(),
    ast::Expr::ExprImport(_) => todo!(),
    ast::Expr::ExprError(_) => todo!(),
  };
  Some(st.expr(data))
}

fn get_id(st: &mut St, id: SyntaxToken) -> Id {
  Id::new(st.str(id.text()))
}

fn get_fn(
  st: &mut St,
  paren_params: ast::ParenParams,
  body: Option<ast::Expr>,
  in_obj: bool,
) -> jsonnet_hir::Expr {
  let mut params = Vec::<(Id, jsonnet_hir::Expr)>::new();
  for param in paren_params.params() {
    let lhs = get_id(st, param.id()?);
    let rhs = match param.eq_expr() {
      Some(rhs) => get_expr(st, rhs.expr(), in_obj),
      None => {
        let msg = ExprData::Prim(Prim::String(Str::PARAMETER_NOT_BOUND));
        let msg = Some(st.expr(msg));
        Some(st.expr(ExprData::Error(msg)))
      }
    };
    params.push((lhs, rhs));
  }
  let body = get_expr(st, body, in_obj);
  Some(st.expr(ExprData::Function { params, body }))
}
