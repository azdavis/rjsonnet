//! The internal impl.

use crate::{error, escape, st::St};
use jsonnet_expr::{BinaryOp, Expr, ExprData, Id, Number, Prim, Str, UnaryOp, Visibility};
use jsonnet_syntax::ast::{self, AstNode as _};
use jsonnet_syntax::kind::SyntaxToken;

pub(crate) fn get_root(st: &mut St, r: ast::Root) -> Expr {
  get_expr(st, r.expr(), false)
}

/// - TODO only allow super/$/tailstrict sometimes?
/// - TODO actually lower strings, numbers
fn get_expr(st: &mut St, expr: Option<ast::Expr>, in_obj: bool) -> Expr {
  let expr = expr?;
  let ptr = ast::SyntaxNodePtr::new(expr.syntax());
  let data = match expr {
    ast::Expr::ExprNull(_) => ExprData::Prim(Prim::Null),
    ast::Expr::ExprTrue(_) => ExprData::Prim(Prim::Bool(true)),
    ast::Expr::ExprFalse(_) => ExprData::Prim(Prim::Bool(false)),
    ast::Expr::ExprSelf(_) => ExprData::Id(Id::SELF),
    ast::Expr::ExprSuper(_) => ExprData::Id(Id::SUPER),
    ast::Expr::ExprDollar(_) => ExprData::Id(Id::DOLLAR),
    ast::Expr::ExprString(expr) => {
      let tok = expr.string()?;
      let string = escape::get(st, tok);
      ExprData::Prim(Prim::String(st.str(string.as_str())))
    }
    ast::Expr::ExprNumber(expr) => {
      let tok = expr.number()?;
      let num: f64 = tok.text().parse().unwrap_or(0.0);
      let num = match Number::try_from(num) {
        Ok(x) => x,
        Err(_) => {
          st.err(&expr, error::Kind::CannotRepresentNumber);
          Number::positive_zero()
        }
      };
      ExprData::Prim(Prim::Number(num))
    }
    ast::Expr::ExprId(expr) => ExprData::Id(get_id(st, expr.id()?)),
    ast::Expr::ExprParen(expr) => return get_expr(st, expr.expr(), in_obj),
    ast::Expr::ExprObject(expr) => get_object(st, expr, in_obj),
    ast::Expr::ExprArray(expr) => match get_comp_specs(st, expr.comp_specs()) {
      Some(_) => {
        let mut expr_commas = expr.expr_commas();
        let Some(elem) = expr_commas.next().and_then(|x| x.expr()) else {
          st.err(&expr, error::Kind::ArrayCompNotOne);
          return None;
        };
        let elem = get_expr(st, Some(elem), in_obj);
        for elem in expr_commas {
          st.err(&elem, error::Kind::ArrayCompNotOne);
        }
        get_array_comp(st, expr.comp_specs(), elem, in_obj)
      }
      None => ExprData::Array(expr.expr_commas().map(|x| get_expr(st, x.expr(), in_obj)).collect()),
    },
    ast::Expr::ExprFieldGet(expr) => {
      let on = get_expr(st, expr.expr(), in_obj);
      let idx = ExprData::Prim(Prim::String(st.str(expr.id()?.text())));
      let idx = Some(st.expr(ptr.clone(), idx));
      ExprData::Subscript { on, idx }
    }
    ast::Expr::ExprSubscript(expr) => {
      let on = get_expr(st, expr.on(), in_obj);
      let is_regular_subscript = expr.idx_a().is_some()
        && expr.colon_a().is_none()
        && expr.idx_b().is_none()
        && expr.colon_b().is_none()
        && expr.idx_c().is_none();
      if is_regular_subscript {
        let idx = get_expr(st, expr.idx_a(), in_obj);
        ExprData::Subscript { on, idx }
      } else {
        let indices = [expr.idx_a(), expr.idx_b(), expr.idx_c()];
        let [idx_a, idx_b, idx_c] =
          indices.map(|idx| get_expr_or_null(st, ptr.clone(), idx, in_obj));
        call_std_func_data(st, ptr.clone(), Id::SLICE, vec![on, idx_a, idx_b, idx_c])
      }
    }
    ast::Expr::ExprCall(expr) => {
      let func = get_expr(st, expr.expr(), in_obj);
      let mut positional = Vec::<Expr>::new();
      let mut named = Vec::<(Id, Expr)>::new();
      for arg in expr.args() {
        let expr = get_expr(st, arg.expr(), in_obj);
        match arg.id_eq().and_then(|x| x.id()) {
          None => positional.push(expr),
          Some(id) => named.push((get_id(st, id), expr)),
        }
      }
      ExprData::Call { func, positional, named }
    }
    ast::Expr::ExprLocal(expr) => {
      let binds: Vec<_> = expr.binds().filter_map(|bind| get_bind(st, bind, in_obj)).collect();
      let body = get_expr(st, expr.expr(), in_obj);
      ExprData::Local { binds, body }
    }
    ast::Expr::ExprIf(expr) => {
      let cond = get_expr(st, expr.cond(), in_obj);
      let yes = get_expr(st, expr.yes(), in_obj);
      let no = get_expr_or_null(st, ptr.clone(), expr.else_expr().and_then(|x| x.expr()), in_obj);
      ExprData::If { cond, yes, no }
    }
    ast::Expr::ExprBinaryOp(expr) => {
      let lhs = get_expr(st, expr.lhs(), in_obj);
      let rhs = get_expr(st, expr.rhs(), in_obj);
      let op = expr.binary_op()?.kind;
      get_binary_op(st, ptr.clone(), lhs, op, rhs)
    }
    ast::Expr::ExprUnaryOp(expr) => {
      let inner = get_expr(st, expr.expr(), in_obj);
      let op = match expr.unary_op()?.kind {
        ast::UnaryOpKind::Minus => UnaryOp::Neg,
        ast::UnaryOpKind::Plus => UnaryOp::Pos,
        ast::UnaryOpKind::Bang => UnaryOp::LogicalNot,
        ast::UnaryOpKind::Tilde => UnaryOp::BitNot,
      };
      ExprData::UnaryOp { op, inner }
    }
    ast::Expr::ExprImplicitObjectPlus(expr) => {
      let lhs = get_expr(st, expr.expr(), in_obj);
      let rhs = expr.expr_object()?;
      let rhs_ptr = ast::SyntaxNodePtr::new(rhs.syntax());
      let rhs = get_object(st, rhs, in_obj);
      let rhs = Some(st.expr(rhs_ptr, rhs));
      bop(BinaryOp::Add, lhs, rhs)
    }
    ast::Expr::ExprFunction(expr) => get_fn(st, expr.paren_params(), expr.expr(), in_obj),
    ast::Expr::ExprAssert(expr) => {
      let yes = get_expr(st, expr.expr(), in_obj);
      get_assert(st, yes, expr.assert()?, in_obj)
    }
    ast::Expr::ExprImport(_) => todo!(),
    ast::Expr::ExprError(expr) => {
      let inner = get_expr(st, expr.expr(), in_obj);
      ExprData::Error(inner)
    }
    ast::Expr::ExprTailstrict(expr) => return get_expr(st, expr.expr(), in_obj),
  };
  Some(st.expr(ptr, data))
}

fn get_expr_or_null(
  st: &mut St,
  ptr: ast::SyntaxNodePtr,
  expr: Option<ast::Expr>,
  in_obj: bool,
) -> Expr {
  if expr.is_some() {
    get_expr(st, expr, in_obj)
  } else {
    Some(st.expr(ptr, ExprData::Prim(Prim::Null)))
  }
}

#[allow(clippy::unnecessary_wraps)]
fn call_std_func(st: &mut St, ptr: ast::SyntaxNodePtr, id: Id, args: Vec<Expr>) -> Expr {
  let data = call_std_func_data(st, ptr.clone(), id, args);
  Some(st.expr(ptr, data))
}

fn call_std_func_data(st: &mut St, ptr: ast::SyntaxNodePtr, id: Id, args: Vec<Expr>) -> ExprData {
  let std = Some(st.expr(ptr.clone(), ExprData::Id(Id::STD_UNUTTERABLE)));
  let idx = Some(st.expr(ptr.clone(), ExprData::Id(id)));
  let func = Some(st.expr(ptr, ExprData::Subscript { on: std, idx }));
  ExprData::Call { func, positional: args, named: Vec::new() }
}

fn get_array_comp<I>(st: &mut St, comp_specs: I, elem: Expr, in_obj: bool) -> ExprData
where
  I: Iterator<Item = ast::CompSpec>,
{
  let comp_specs: Vec<_> = comp_specs.collect();
  comp_specs.into_iter().rev().fold(ExprData::Array(vec![elem]), |ac, comp_spec| {
    let ptr = ast::SyntaxNodePtr::new(comp_spec.syntax());
    let empty_array = Some(st.expr(ptr.clone(), ExprData::Array(Vec::new())));
    match comp_spec {
      ast::CompSpec::ForSpec(for_spec) => {
        // skip this `for` if no var for the `for`
        let Some(for_var) = for_spec.id() else { return ac };
        let ac = Some(st.expr(ptr.clone(), ac));
        let for_var = get_id(st, for_var);
        let in_expr = get_expr(st, for_spec.expr(), in_obj);
        let arr = st.fresh();
        let idx = st.fresh();
        let arr_expr = Some(st.expr(ptr.clone(), ExprData::Id(arr)));
        let idx_expr = Some(st.expr(ptr.clone(), ExprData::Id(idx)));
        let length = call_std_func(st, ptr.clone(), Id::LENGTH, vec![arr_expr]);
        let subscript =
          Some(st.expr(ptr.clone(), ExprData::Subscript { on: arr_expr, idx: idx_expr }));
        let recur_with_subscript = ExprData::Local { binds: vec![(for_var, subscript)], body: ac };
        let recur_with_subscript = Some(st.expr(ptr.clone(), recur_with_subscript));
        let unbound_err = err_param_unbound(st, ptr.clone());
        let lambda =
          ExprData::Function { params: vec![(idx, unbound_err)], body: recur_with_subscript };
        let lambda_recur_with_subscript = Some(st.expr(ptr.clone(), lambda));
        let make_array =
          call_std_func(st, ptr.clone(), Id::MAKE_ARRAY, vec![length, lambda_recur_with_subscript]);
        let join = call_std_func(st, ptr, Id::JOIN, vec![empty_array, make_array]);
        ExprData::Local { binds: vec![(arr, in_expr)], body: join }
      }
      ast::CompSpec::IfSpec(if_spec) => {
        let ac = Some(st.expr(ptr, ac));
        let cond = get_expr(st, if_spec.expr(), in_obj);
        ExprData::If { cond, yes: ac, no: empty_array }
      }
    }
  })
}

fn get_id(st: &mut St, id: SyntaxToken) -> Id {
  Id::new(st.str(id.text()))
}

fn get_bind(st: &mut St, bind: ast::Bind, in_obj: bool) -> Option<(Id, Expr)> {
  let lhs = get_id(st, bind.id()?);
  let rhs = bind.expr();
  let rhs = match bind.paren_params() {
    None => get_expr(st, rhs, in_obj),
    Some(params) => {
      let ptr = ast::SyntaxNodePtr::new(params.syntax());
      let fn_data = get_fn(st, Some(params), rhs, in_obj);
      Some(st.expr(ptr, fn_data))
    }
  };
  Some((lhs, rhs))
}

#[allow(clippy::unnecessary_wraps)]
fn err_param_unbound(st: &mut St, ptr: ast::SyntaxNodePtr) -> Expr {
  let msg = ExprData::Prim(Prim::String(Str::PARAMETER_NOT_BOUND));
  let msg = Some(st.expr(ptr.clone(), msg));
  Some(st.expr(ptr, ExprData::Error(msg)))
}

fn get_fn(
  st: &mut St,
  paren_params: Option<ast::ParenParams>,
  body: Option<ast::Expr>,
  in_obj: bool,
) -> ExprData {
  let mut params = Vec::<(Id, Expr)>::new();
  for param in paren_params.into_iter().flat_map(|x| x.params()) {
    let Some(lhs) = param.id() else { continue };
    let lhs = get_id(st, lhs);
    let rhs = match param.eq_expr() {
      Some(rhs) => get_expr(st, rhs.expr(), in_obj),
      None => {
        let ptr = ast::SyntaxNodePtr::new(param.syntax());
        err_param_unbound(st, ptr)
      }
    };
    params.push((lhs, rhs));
  }
  let body = get_expr(st, body, in_obj);
  ExprData::Function { params, body }
}

fn get_comp_specs<I>(st: &mut St, mut iter: I) -> Option<(ast::ForSpec, I)>
where
  I: Iterator<Item = ast::CompSpec>,
{
  match iter.next()? {
    ast::CompSpec::ForSpec(for_spec) => Some((for_spec, iter)),
    ast::CompSpec::IfSpec(if_spec) => {
      st.err(&if_spec, error::Kind::FirstCompSpecNotFor);
      None
    }
  }
}

fn get_assert(st: &mut St, yes: Expr, assert: ast::Assert, in_obj: bool) -> ExprData {
  let cond = get_expr(st, assert.expr(), in_obj);
  let ptr = ast::SyntaxNodePtr::new(assert.syntax());
  let msg = match assert.colon_expr() {
    Some(expr) => get_expr(st, expr.expr(), in_obj),
    None => Some(st.expr(ptr.clone(), ExprData::Prim(Prim::String(Str::ASSERTION_FAILED)))),
  };
  let no = Some(st.expr(ptr, ExprData::Error(msg)));
  ExprData::If { cond, yes, no }
}

fn get_object(st: &mut St, inside: ast::ExprObject, in_obj: bool) -> ExprData {
  match get_comp_specs(st, inside.comp_specs()) {
    None => {
      // this is the only time we actually use the `in_obj` flag
      let mut asserts = Vec::<Expr>::new();
      let mut fields = Vec::<(Expr, Visibility, Expr)>::new();
      for member in inside.members() {
        let Some(member_kind) = member.member_kind() else { continue };
        match member_kind {
          ast::MemberKind::ObjectLocal(_) => {
            todo!()
          }
          ast::MemberKind::Assert(assert) => {
            let ptr = ast::SyntaxNodePtr::new(assert.syntax());
            let yes = Some(st.expr(ptr.clone(), ExprData::Prim(Prim::Null)));
            let assert = get_assert(st, yes, assert, true);
            let assert = Some(st.expr(ptr, assert));
            // TODO handle interactions with binds
            asserts.push(assert);
          }
          ast::MemberKind::Field(field) => {
            let name = match field.field_name() {
              None => continue,
              Some(ast::FieldName::FieldNameId(name)) => match name.id() {
                Some(id) => {
                  let expr = ExprData::Prim(Prim::String(st.str(id.text())));
                  let ptr = ast::SyntaxNodePtr::new(name.syntax());
                  Some(st.expr(ptr, expr))
                }
                None => continue,
              },
              Some(ast::FieldName::FieldNameString(name)) => match name.string() {
                Some(string) => {
                  let expr = ExprData::Prim(Prim::String(st.str(string.text())));
                  let ptr = ast::SyntaxNodePtr::new(name.syntax());
                  Some(st.expr(ptr, expr))
                }
                None => continue,
              },
              Some(ast::FieldName::FieldNameExpr(name)) => get_expr(st, name.expr(), in_obj),
            };
            let body = match field.field_extra() {
              None => get_expr(st, field.expr(), true),
              Some(ast::FieldExtra::FieldPlus(_)) => todo!(),
              Some(ast::FieldExtra::ParenParams(paren_params)) => {
                let ptr = ast::SyntaxNodePtr::new(paren_params.syntax());
                let expr = get_fn(st, Some(paren_params), field.expr(), true);
                Some(st.expr(ptr, expr))
              }
            };
            let vis = match field.visibility() {
              Some(vis) => match vis.kind {
                ast::VisibilityKind::Colon => Visibility::Default,
                ast::VisibilityKind::ColonColon => Visibility::Hidden,
                ast::VisibilityKind::ColonColonColon => Visibility::Visible,
              },
              None => Visibility::Default,
            };
            // TODO handle interactions with binds
            fields.push((name, vis, body));
          }
        }
      }
      ExprData::Object { asserts, fields }
    }
    Some((_, comp_specs)) => {
      let mut binds = Vec::<(Id, Expr)>::new();
      let mut lowered_field = None::<(ast::SyntaxNodePtr, Expr, Expr)>;
      for member in inside.members() {
        let Some(member_kind) = member.member_kind() else { continue };
        match member_kind {
          ast::MemberKind::ObjectLocal(local) => {
            binds.extend(local.bind().and_then(|b| get_bind(st, b, true)));
          }
          ast::MemberKind::Assert(assert) => {
            st.err(&assert, error::Kind::ObjectCompAssert);
          }
          ast::MemberKind::Field(field) => {
            let Some(name) = field.field_name() else {
              continue;
            };
            let ast::FieldName::FieldNameExpr(name) = name else {
              st.err(&name, error::Kind::ObjectCompLiteralFieldName);
              continue;
            };
            if lowered_field.is_some() {
              st.err(&field, error::Kind::ObjectCompNotOne);
            }
            if let Some(field_extra) = field.field_extra() {
              st.err(&field_extra, error::Kind::ObjectCompFieldExtra);
            }
            if let Some(vis) = field.visibility() {
              let is_colon = matches!(vis.kind, ast::VisibilityKind::Colon);
              if !is_colon {
                st.err_token(vis.token, error::Kind::ObjectCompVisibility);
              }
            }
            let name = get_expr(st, name.expr(), in_obj);
            let body = get_expr(st, field.expr(), in_obj);
            let ptr = ast::SyntaxNodePtr::new(field.syntax());
            lowered_field = Some((ptr, name, body));
          }
        }
      }
      let vars = comp_specs.filter_map(|comp_spec| match comp_spec {
        ast::CompSpec::ForSpec(spec) => {
          spec.id().map(|x| (ast::SyntaxNodePtr::new(spec.syntax()), get_id(st, x)))
        }
        ast::CompSpec::IfSpec(_) => None,
      });
      let vars: Vec<_> = vars.collect();
      match lowered_field {
        None => {
          st.err(&inside, error::Kind::ObjectCompNotOne);
          // this is a good "fake" return, since we knew this was going to be some kind of object,
          // but now we can't figure out what fields it should have. so let's say it has no fields.
          ExprData::Object { asserts: Vec::new(), fields: Vec::new() }
        }
        Some((ptr, name, body)) => {
          let arr = st.fresh();
          let on = Some(st.expr(ptr.clone(), ExprData::Id(arr)));
          let name_binds = vars.iter().enumerate().map(|(idx, (ptr, id))| {
            let idx = u32::try_from(idx).unwrap();
            let idx = f64::try_from(idx).unwrap();
            let idx = Number::try_from(idx).expect("infinite array idx");
            let idx = Some(st.expr(ptr.clone(), ExprData::Prim(Prim::Number(idx))));
            let subscript = Some(st.expr(ptr.clone(), ExprData::Subscript { on, idx }));
            (*id, subscript)
          });
          let name_binds: Vec<_> = name_binds.collect();
          let body_binds: Vec<_> = name_binds.iter().copied().chain(binds).collect();
          let name = Some(st.expr(ptr.clone(), ExprData::Local { binds: name_binds, body: name }));
          let body = Some(st.expr(ptr.clone(), ExprData::Local { binds: body_binds, body }));
          let vars = vars.into_iter().map(|(ptr, x)| Some(st.expr(ptr, ExprData::Id(x))));
          let vars: Vec<_> = vars.collect();
          let vars_ary = Some(st.expr(ptr, ExprData::Array(vars)));
          // TODO is this right?
          ExprData::ObjectComp { name, body, id: arr, ary: vars_ary }
        }
      }
    }
  }
}

fn bop(op: BinaryOp, lhs: Expr, rhs: Expr) -> ExprData {
  ExprData::BinaryOp { lhs, op, rhs }
}

fn get_binary_op(
  st: &mut St,
  ptr: ast::SyntaxNodePtr,
  lhs: Expr,
  op: ast::BinaryOpKind,
  rhs: Expr,
) -> ExprData {
  match op {
    ast::BinaryOpKind::Star => bop(BinaryOp::Mul, lhs, rhs),
    ast::BinaryOpKind::Slash => bop(BinaryOp::Div, lhs, rhs),
    ast::BinaryOpKind::Percent => call_std_func_data(st, ptr, Id::MOD, vec![lhs, rhs]),
    ast::BinaryOpKind::Plus => bop(BinaryOp::Add, lhs, rhs),
    ast::BinaryOpKind::Minus => bop(BinaryOp::Sub, lhs, rhs),
    ast::BinaryOpKind::LtLt => bop(BinaryOp::Shl, lhs, rhs),
    ast::BinaryOpKind::GtGt => bop(BinaryOp::Shr, lhs, rhs),
    ast::BinaryOpKind::Lt => bop(BinaryOp::Lt, lhs, rhs),
    ast::BinaryOpKind::LtEq => bop(BinaryOp::LtEq, lhs, rhs),
    ast::BinaryOpKind::Gt => bop(BinaryOp::Gt, lhs, rhs),
    ast::BinaryOpKind::GtEq => bop(BinaryOp::GtEq, lhs, rhs),
    ast::BinaryOpKind::EqEq => call_std_func_data(st, ptr, Id::EQUALS, vec![lhs, rhs]),
    ast::BinaryOpKind::BangEq => {
      let inner = call_std_func(st, ptr, Id::EQUALS, vec![lhs, rhs]);
      ExprData::UnaryOp { op: UnaryOp::LogicalNot, inner }
    }
    ast::BinaryOpKind::InKw => call_std_func_data(st, ptr, Id::OBJECT_HAS_EX, vec![lhs, rhs]),
    ast::BinaryOpKind::And => bop(BinaryOp::BitAnd, lhs, rhs),
    ast::BinaryOpKind::Carat => bop(BinaryOp::BitXor, lhs, rhs),
    ast::BinaryOpKind::Bar => bop(BinaryOp::BitOr, lhs, rhs),
    ast::BinaryOpKind::AndAnd => bop(BinaryOp::LogicalAnd, lhs, rhs),
    ast::BinaryOpKind::BarBar => bop(BinaryOp::LogicalOr, lhs, rhs),
  }
}
