//! The internal impl.
//!
//! TODO how to construct arbitrary expressions not from the source code that we can then desugar?

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
    ast::Expr::ExprObject(e) => {
      let inside = e.object_inside()?;
      let mut comp_specs = inside.comp_specs();
      match comp_specs.next() {
        None => todo!(),
        Some(comp_spec) => {
          match comp_spec {
            ast::CompSpec::ForSpec(for_spec) => {}
            ast::CompSpec::IfSpec(if_spec) => {
              st.err(&if_spec, "the first comprehension specification must not be `if`");
            }
          }
          let mut binds = Vec::<(Id, jsonnet_hir::Expr)>::new();
          let mut lowered_field = None::<(jsonnet_hir::Expr, jsonnet_hir::Expr)>;
          for member in inside.members() {
            match member.member_kind()? {
              ast::MemberKind::ObjectLocal(local) => {
                binds.extend(local.bind().and_then(|b| get_bind(st, b, true)));
              }
              ast::MemberKind::Assert(assert) => {
                st.err(&assert, "object comprehension must not contain asserts");
              }
              ast::MemberKind::Field(field) => match field.field_name()? {
                ast::FieldName::FieldNameExpr(field_name) => match lowered_field {
                  None => {
                    if let Some(field_extra) = field.field_extra() {
                      st.err(
                        &field_extra,
                        "object comprehension field must not have `+` or parameters",
                      );
                    }
                    let vis = field.visibility()?;
                    let is_colon = matches!(vis.kind, ast::VisibilityKind::Colon);
                    if !is_colon {
                      st.err_token(vis.token, "object comprehension field must use `:`");
                    }
                    let name = get_expr(st, field_name.expr(), in_obj);
                    let body = get_expr(st, field.expr(), in_obj);
                    lowered_field = Some((name, body));
                  }
                  Some(_) => {
                    st.err(&field, "object comprehension must not contain more than 1 field");
                  }
                },
                non_expr_name => {
                  st.err(
                    &non_expr_name,
                    "object comprehension must not contain literal field names",
                  );
                }
              },
            };
          }
          let vars = comp_specs.filter_map(|comp_spec| match comp_spec {
            ast::CompSpec::ForSpec(spec) => Some(get_id(st, spec.id()?)),
            ast::CompSpec::IfSpec(_) => None,
          });
          let vars: Vec<_> = vars.collect();
          match lowered_field {
            None => {
              st.err(&inside, "object comprehension must contain a field");
              return None;
            }
            Some((name, body)) => {
              let arr = st.fresh();
              let on = Some(st.expr(ExprData::Id(arr)));
              let name_binds = vars.iter().enumerate().map(|(idx, &id)| {
                let idx = u32::try_from(idx).unwrap();
                let idx = f64::try_from(idx).unwrap();
                let idx = Some(st.expr(ExprData::Prim(Prim::Number(idx))));
                let subscript = Some(st.expr(ExprData::Subscript { on, idx }));
                (id, subscript)
              });
              let name_binds: Vec<_> = name_binds.collect();
              let body_binds: Vec<_> = name_binds.iter().copied().chain(binds).collect();
              let name = Some(st.expr(ExprData::Local { binds: name_binds, body: name }));
              let body = Some(st.expr(ExprData::Local { binds: body_binds, body }));
              let vars = vars.into_iter().map(|x| Some(st.expr(ExprData::Id(x))));
              let vars: Vec<_> = vars.collect();
              let arr = Some(st.expr(ExprData::Array(vars)));
              // ExprData::ObjectComp { name, body, id: arr, ary: () }
              todo!()
            }
          }
        }
      }
    }
    ast::Expr::ExprArray(_) => todo!(),
    ast::Expr::ExprFieldGet(_) => todo!(),
    ast::Expr::ExprSubscript(_) => todo!(),
    ast::Expr::ExprCall(_) => todo!(),
    ast::Expr::ExprLocal(e) => {
      let binds: Vec<_> = e.binds().filter_map(|bind| get_bind(st, bind, in_obj)).collect();
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

fn get_bind(st: &mut St, bind: ast::Bind, in_obj: bool) -> Option<(Id, jsonnet_hir::Expr)> {
  let lhs = get_id(st, bind.id()?);
  let rhs = bind.expr();
  let rhs = match bind.paren_params() {
    None => get_expr(st, rhs, in_obj),
    Some(params) => get_fn(st, params, rhs, in_obj),
  };
  Some((lhs, rhs))
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
