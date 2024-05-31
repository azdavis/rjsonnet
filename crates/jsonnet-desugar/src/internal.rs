//! The internal impl.

use crate::{cx::Cx, error, st::St};
use jsonnet_expr::{
  BinaryOp, Expr, ExprData, Id, ImportKind, Number, Prim, Str, UnaryOp, Visibility,
};
use jsonnet_syntax::ast::{self, AstNode as _};

/// TODO only allow super/$ sometimes?
pub(crate) fn get_expr(st: &mut St, cx: Cx<'_>, expr: Option<ast::Expr>, in_obj: bool) -> Expr {
  let expr = expr?;
  let ptr = ast::SyntaxNodePtr::new(expr.syntax());
  let data = match expr {
    ast::Expr::ExprNull(_) => ExprData::Prim(Prim::Null),
    ast::Expr::ExprTrue(_) => ExprData::Prim(Prim::Bool(true)),
    ast::Expr::ExprFalse(_) => ExprData::Prim(Prim::Bool(false)),
    ast::Expr::ExprSelf(_) => ExprData::Id(Id::self_),
    ast::Expr::ExprSuper(_) => ExprData::Id(Id::super_),
    ast::Expr::ExprDollar(_) => ExprData::Id(Id::dollar),
    ast::Expr::ExprString(expr) => {
      let string = expr.string()?;
      let string = jsonnet_ast_escape::get(&string);
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
    ast::Expr::ExprId(expr) => ExprData::Id(st.id(expr.id()?)),
    ast::Expr::ExprParen(expr) => return get_expr(st, cx, expr.expr(), in_obj),
    ast::Expr::ExprObject(expr) => get_object(st, cx, expr.object()?, in_obj),
    ast::Expr::ExprArray(expr) => {
      if let Some(spec) = expr.comp_specs().next() {
        match spec {
          ast::CompSpec::ForSpec(_) => {}
          ast::CompSpec::IfSpec(_) => {
            st.err(&spec, error::Kind::FirstCompSpecNotFor);
          }
        }
        let mut expr_commas = expr.expr_commas();
        let Some(elem) = expr_commas.next().and_then(|x| x.expr()) else {
          st.err(&expr, error::Kind::ArrayCompNotOne);
          return None;
        };
        let elem = get_expr(st, cx, Some(elem), in_obj);
        for elem in expr_commas {
          st.err(&elem, error::Kind::ArrayCompNotOne);
        }
        get_array_comp(st, cx, expr.comp_specs(), elem, in_obj)
      } else {
        ExprData::Array(expr.expr_commas().map(|x| get_expr(st, cx, x.expr(), in_obj)).collect())
      }
    }
    ast::Expr::ExprFieldGet(expr) => {
      let on = get_expr(st, cx, expr.expr(), in_obj);
      let idx = ExprData::Prim(Prim::String(st.str(expr.id()?.text())));
      let idx = Some(st.expr(ptr, idx));
      ExprData::Subscript { on, idx }
    }
    ast::Expr::ExprSubscript(expr) => {
      let on = get_expr(st, cx, expr.on(), in_obj);
      let is_regular_subscript = expr.idx_a().is_some()
        && expr.colon_a().is_none()
        && expr.idx_b().is_none()
        && expr.colon_b().is_none()
        && expr.idx_c().is_none();
      if is_regular_subscript {
        let idx = get_expr(st, cx, expr.idx_a(), in_obj);
        ExprData::Subscript { on, idx }
      } else {
        let indices = [expr.idx_a(), expr.idx_b(), expr.idx_c()];
        let [idx_a, idx_b, idx_c] = indices.map(|idx| get_expr_or_null(st, cx, ptr, idx, in_obj));
        call_std_func_data(st, ptr, Str::slice, vec![on, idx_a, idx_b, idx_c])
      }
    }
    ast::Expr::ExprCall(expr) => {
      let func = get_expr(st, cx, expr.expr(), in_obj);
      let mut positional = Vec::<Expr>::new();
      let mut named = Vec::<(Id, Expr)>::new();
      let mut saw_named = false;
      for arg in expr.args() {
        let expr = get_expr(st, cx, arg.expr(), in_obj);
        match arg.id_eq().and_then(|x| x.id()) {
          None => {
            if saw_named {
              st.err(&arg, error::Kind::PositionalArgAfterNamedArg);
            }
            positional.push(expr);
          }
          Some(id) => {
            saw_named = true;
            named.push((st.id(id), expr));
          }
        }
      }
      ExprData::Call { func, positional, named }
    }
    ast::Expr::ExprLocal(expr) => {
      let binds: Vec<_> =
        expr.bind_commas().filter_map(|bind| get_bind(st, cx, bind.bind()?, in_obj)).collect();
      let body = get_expr(st, cx, expr.expr(), in_obj);
      ExprData::Local { binds, body }
    }
    ast::Expr::ExprIf(expr) => {
      let cond = get_expr(st, cx, expr.cond(), in_obj);
      let yes = get_expr(st, cx, expr.yes(), in_obj);
      let no = get_expr_or_null(st, cx, ptr, expr.else_expr().and_then(|x| x.expr()), in_obj);
      ExprData::If { cond, yes, no }
    }
    ast::Expr::ExprBinaryOp(expr) => {
      let lhs = get_expr(st, cx, expr.lhs(), in_obj);
      let rhs = get_expr(st, cx, expr.rhs(), in_obj);
      let op = expr.binary_op()?.kind;
      get_binary_op(st, ptr, lhs, op, rhs)
    }
    ast::Expr::ExprUnaryOp(expr) => {
      let inner = get_expr(st, cx, expr.expr(), in_obj);
      let op = match expr.unary_op()?.kind {
        ast::UnaryOpKind::Minus => UnaryOp::Neg,
        ast::UnaryOpKind::Plus => UnaryOp::Pos,
        ast::UnaryOpKind::Bang => UnaryOp::LogicalNot,
        ast::UnaryOpKind::Tilde => UnaryOp::BitNot,
      };
      ExprData::UnaryOp { op, inner }
    }
    ast::Expr::ExprImplicitObjectPlus(expr) => {
      let lhs = get_expr(st, cx, expr.expr(), in_obj);
      let rhs = expr.object()?;
      let rhs_ptr = ast::SyntaxNodePtr::new(rhs.syntax());
      let rhs = get_object(st, cx, rhs, in_obj);
      let rhs = Some(st.expr(rhs_ptr, rhs));
      bop(BinaryOp::Add, lhs, rhs)
    }
    ast::Expr::ExprFunction(expr) => get_fn(st, cx, expr.paren_params(), expr.expr(), in_obj),
    ast::Expr::ExprAssert(expr) => {
      let yes = get_expr(st, cx, expr.expr(), in_obj);
      get_assert(st, cx, yes, expr.assert()?, in_obj)
    }
    ast::Expr::ExprImport(expr) => {
      let kind = match expr.import()?.kind {
        ast::ImportKind::ImportKw => ImportKind::Code,
        ast::ImportKind::ImportstrKw => ImportKind::String,
        ast::ImportKind::ImportbinKw => ImportKind::Binary,
      };
      let import_str = expr.string()?;
      let import_str = jsonnet_ast_escape::get(&import_str);
      let import_path = std::path::Path::new(import_str.as_str());
      let full_path = cx.dirs().find_map(|dir| {
        let path = dir.join(import_path);
        cx.fs.is_file(path.as_path()).then_some(path)
      });
      match full_path {
        Some(p) => ExprData::Import { kind, path: st.path_id(p.as_clean_path()) },
        None => {
          st.err(&expr, error::Kind::PathNotFound(import_str));
          return None;
        }
      }
    }
    ast::Expr::ExprError(expr) => {
      let inner = get_expr(st, cx, expr.expr(), in_obj);
      ExprData::Error(inner)
    }
  };
  Some(st.expr(ptr, data))
}

fn get_expr_or_null(
  st: &mut St,
  cx: Cx<'_>,
  ptr: ast::SyntaxNodePtr,
  expr: Option<ast::Expr>,
  in_obj: bool,
) -> Expr {
  if expr.is_some() {
    get_expr(st, cx, expr, in_obj)
  } else {
    Some(st.expr(ptr, ExprData::Prim(Prim::Null)))
  }
}

#[allow(clippy::unnecessary_wraps)]
fn call_std_func(st: &mut St, ptr: ast::SyntaxNodePtr, name: Str, args: Vec<Expr>) -> Expr {
  let data = call_std_func_data(st, ptr, name, args);
  Some(st.expr(ptr, data))
}

fn call_std_func_data(
  st: &mut St,
  ptr: ast::SyntaxNodePtr,
  name: Str,
  args: Vec<Expr>,
) -> ExprData {
  let std = Some(st.expr(ptr, ExprData::Id(Id::std_unutterable)));
  let idx = Some(st.expr(ptr, ExprData::Prim(Prim::String(name))));
  let func = Some(st.expr(ptr, ExprData::Subscript { on: std, idx }));
  ExprData::Call { func, positional: args, named: Vec::new() }
}

fn get_array_comp<I>(st: &mut St, cx: Cx<'_>, comp_specs: I, elem: Expr, in_obj: bool) -> ExprData
where
  I: Iterator<Item = ast::CompSpec>,
{
  let comp_specs: Vec<_> = comp_specs.collect();
  comp_specs.into_iter().rev().fold(ExprData::Array(vec![elem]), |ac, comp_spec| {
    let ptr = ast::SyntaxNodePtr::new(comp_spec.syntax());
    let empty_array = Some(st.expr(ptr, ExprData::Array(Vec::new())));
    match comp_spec {
      ast::CompSpec::ForSpec(for_spec) => {
        // skip this `for` if no var for the `for`
        let Some(for_var) = for_spec.id() else { return ac };
        let ac = Some(st.expr(ptr, ac));
        let for_var = st.id(for_var);
        let in_expr = get_expr(st, cx, for_spec.expr(), in_obj);
        let arr = st.fresh();
        let idx = st.fresh();
        let arr_expr = Some(st.expr(ptr, ExprData::Id(arr)));
        let idx_expr = Some(st.expr(ptr, ExprData::Id(idx)));
        let length = call_std_func(st, ptr, Str::length, vec![arr_expr]);
        let subscript = Some(st.expr(ptr, ExprData::Subscript { on: arr_expr, idx: idx_expr }));
        let recur_with_subscript = ExprData::Local { binds: vec![(for_var, subscript)], body: ac };
        let recur_with_subscript = Some(st.expr(ptr, recur_with_subscript));
        let lambda = ExprData::Function { params: vec![(idx, None)], body: recur_with_subscript };
        let lambda_recur_with_subscript = Some(st.expr(ptr, lambda));
        let make_array =
          call_std_func(st, ptr, Str::makeArray, vec![length, lambda_recur_with_subscript]);
        let join = call_std_func(st, ptr, Str::join, vec![empty_array, make_array]);
        ExprData::Local { binds: vec![(arr, in_expr)], body: join }
      }
      ast::CompSpec::IfSpec(if_spec) => {
        let ac = Some(st.expr(ptr, ac));
        let cond = get_expr(st, cx, if_spec.expr(), in_obj);
        ExprData::If { cond, yes: ac, no: empty_array }
      }
    }
  })
}

fn get_bind(st: &mut St, cx: Cx<'_>, bind: ast::Bind, in_obj: bool) -> Option<(Id, Expr)> {
  let lhs = st.id(bind.id()?);
  let rhs = bind.expr();
  let rhs = match bind.paren_params() {
    None => get_expr(st, cx, rhs, in_obj),
    Some(params) => {
      let ptr = ast::SyntaxNodePtr::new(params.syntax());
      let fn_data = get_fn(st, cx, Some(params), rhs, in_obj);
      Some(st.expr(ptr, fn_data))
    }
  };
  Some((lhs, rhs))
}

fn get_fn(
  st: &mut St,
  cx: Cx<'_>,
  paren_params: Option<ast::ParenParams>,
  body: Option<ast::Expr>,
  in_obj: bool,
) -> ExprData {
  let mut params = Vec::<(Id, Option<Expr>)>::new();
  for param in paren_params.into_iter().flat_map(|x| x.params()) {
    let Some(lhs) = param.id() else { continue };
    let lhs = st.id(lhs);
    let rhs = param.eq_expr().map(|rhs| get_expr(st, cx, rhs.expr(), in_obj));
    params.push((lhs, rhs));
  }
  let body = get_expr(st, cx, body, in_obj);
  ExprData::Function { params, body }
}

fn get_assert(st: &mut St, cx: Cx<'_>, yes: Expr, assert: ast::Assert, in_obj: bool) -> ExprData {
  let cond = get_expr(st, cx, assert.expr(), in_obj);
  let ptr = ast::SyntaxNodePtr::new(assert.syntax());
  let msg = match assert.colon_expr() {
    Some(expr) => get_expr(st, cx, expr.expr(), in_obj),
    None => Some(st.expr(ptr, ExprData::Prim(Prim::String(Str::ASSERTION_FAILED)))),
  };
  let no = Some(st.expr(ptr, ExprData::Error(msg)));
  ExprData::If { cond, yes, no }
}

fn get_object(st: &mut St, cx: Cx<'_>, inside: ast::Object, in_obj: bool) -> ExprData {
  if let Some(spec) = inside.comp_specs().next() {
    match spec {
      ast::CompSpec::ForSpec(_) => {}
      ast::CompSpec::IfSpec(_) => {
        st.err(&spec, error::Kind::FirstCompSpecNotFor);
      }
    }
    get_object_comp(st, cx, inside, in_obj)
  } else {
    get_object_literal(st, cx, inside, in_obj)
  }
}

/// NOTE: it appears that this is the only place we clone a bunch of binds. this means that "warn on
/// unused vars" won't be able to work without major annoyance.
///
/// the reason is because we have many bindings duplicated for each field/assert, due to the
/// desugaring strategy laid out in the spec for objects. but all those cloned bindings really
/// correspond to only one user-written var. so that user-written var should be marked unused if and
/// only if every copy of it was unused.
///
/// which means we have to do something like... track how many times the var was cloned by keeping a
/// counter, then stick that counter somewhere so later when statics counts how many vars were
/// unused, it knows how many copies to count as unused before marking the actual var as unused...?
fn get_object_literal(st: &mut St, cx: Cx<'_>, inside: ast::Object, in_obj: bool) -> ExprData {
  // first get the binds
  let mut binds = Vec::<(Id, Expr)>::new();
  for member in inside.members() {
    let Some(member_kind) = member.member_kind() else { continue };
    let ast::MemberKind::ObjectLocal(local) = member_kind else { continue };
    binds.extend(local.bind().and_then(|b| get_bind(st, cx, b, true)));
  }
  // this is the only time we actually use the `in_obj` flag
  if !in_obj {
    let ptr = ast::SyntaxNodePtr::new(inside.syntax());
    let this = Some(st.expr(ptr, ExprData::Id(Id::self_)));
    binds.push((Id::dollar, this));
  }
  // then get the asserts and fields
  let mut asserts = Vec::<Expr>::new();
  let mut fields = Vec::<jsonnet_expr::Field>::new();
  for member in inside.members() {
    let Some(member_kind) = member.member_kind() else { continue };
    match member_kind {
      // already did locals
      ast::MemberKind::ObjectLocal(_) => {}
      ast::MemberKind::Assert(assert) => {
        let ptr = ast::SyntaxNodePtr::new(assert.syntax());
        let yes = Some(st.expr(ptr, ExprData::Prim(Prim::Null)));
        let assert = get_assert(st, cx, yes, assert, true);
        let mut assert = Some(st.expr(ptr, assert));
        if !binds.is_empty() {
          assert = Some(st.expr(ptr, ExprData::Local { binds: binds.clone(), body: assert }));
        }
        asserts.push(assert);
      }
      ast::MemberKind::Field(field) => {
        let key = match field.field_name() {
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
              let string = jsonnet_ast_escape::get(&string);
              let expr = ExprData::Prim(Prim::String(st.str(string.as_str())));
              let ptr = ast::SyntaxNodePtr::new(name.syntax());
              Some(st.expr(ptr, expr))
            }
            None => continue,
          },
          Some(ast::FieldName::FieldNameExpr(name)) => get_expr(st, cx, name.expr(), in_obj),
        };
        let vis = match field.visibility() {
          Some(vis) => match vis.kind {
            ast::VisibilityKind::Colon => Visibility::Default,
            ast::VisibilityKind::ColonColon => Visibility::Hidden,
            ast::VisibilityKind::ColonColonColon => Visibility::Visible,
          },
          None => Visibility::Default,
        };
        let (plus, mut val) = match field.field_extra() {
          None => (false, get_expr(st, cx, field.expr(), true)),
          Some(ast::FieldExtra::FieldPlus(_)) => {
            // TODO: the spec says do substitution with self and super and outerself and outersuper,
            // and afterwards remove the distinction between regular fields and field-plus fields,
            // when desugaring. instead, maybe something with the same effect would be to carry
            // through the field-plus-ness of the field past desugaring (as we do here), and then
            // during evaluation, have the env have another 'this' field for the 'outer this', and
            // we could set that up correctly in the corresponding place in the spec where we set
            // outerself and outersuper. then when evaluating "field plus" fields we would replace
            // the regular 'this' with that 'outer this'.
            (true, get_expr(st, cx, field.expr(), true))
          }
          Some(ast::FieldExtra::ParenParams(paren_params)) => {
            let ptr = ast::SyntaxNodePtr::new(paren_params.syntax());
            let expr = get_fn(st, cx, Some(paren_params), field.expr(), true);
            (false, Some(st.expr(ptr, expr)))
          }
        };
        if !binds.is_empty() {
          let ptr = ast::SyntaxNodePtr::new(field.syntax());
          val = Some(st.expr(ptr, ExprData::Local { binds: binds.clone(), body: val }));
        }
        fields.push(jsonnet_expr::Field { key, plus, vis, val });
      }
    }
  }
  ExprData::Object { asserts, fields }
}

fn get_object_comp(st: &mut St, cx: Cx<'_>, inside: ast::Object, in_obj: bool) -> ExprData {
  let mut binds = Vec::<(Id, Expr)>::new();
  let mut lowered_field = None::<(ast::SyntaxNodePtr, Expr, Expr)>;
  for member in inside.members() {
    let Some(member_kind) = member.member_kind() else { continue };
    match member_kind {
      ast::MemberKind::ObjectLocal(local) => {
        binds.extend(local.bind().and_then(|b| get_bind(st, cx, b, true)));
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
        let name = get_expr(st, cx, name.expr(), in_obj);
        let body = get_expr(st, cx, field.expr(), in_obj);
        let ptr = ast::SyntaxNodePtr::new(field.syntax());
        lowered_field = Some((ptr, name, body));
      }
    }
  }
  let vars = inside.comp_specs().filter_map(|comp_spec| match comp_spec {
    ast::CompSpec::ForSpec(spec) => {
      spec.id().map(|x| (ast::SyntaxNodePtr::new(spec.syntax()), st.id(x)))
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
      let on = Some(st.expr(ptr, ExprData::Id(arr)));
      let name_binds = vars.iter().enumerate().map(|(idx, (ptr, id))| {
        let idx = always::convert::usize_to_u32(idx);
        let idx = f64::from(idx);
        let idx = Number::always_from_f64(idx);
        let idx = Some(st.expr(*ptr, ExprData::Prim(Prim::Number(idx))));
        let subscript = Some(st.expr(*ptr, ExprData::Subscript { on, idx }));
        (*id, subscript)
      });
      let name_binds: Vec<_> = name_binds.collect();
      let body_binds: Vec<_> = name_binds.iter().copied().chain(binds).collect();
      let name = Some(st.expr(ptr, ExprData::Local { binds: name_binds, body: name }));
      let body = Some(st.expr(ptr, ExprData::Local { binds: body_binds, body }));
      let vars = vars.into_iter().map(|(ptr, x)| Some(st.expr(ptr, ExprData::Id(x))));
      let vars: Vec<_> = vars.collect();
      let vars = Some(st.expr(ptr, ExprData::Array(vars)));
      let vars = get_array_comp(st, cx, inside.comp_specs(), vars, in_obj);
      let vars = Some(st.expr(ptr, vars));
      ExprData::ObjectComp { name, body, id: arr, ary: vars }
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
    ast::BinaryOpKind::Percent => call_std_func_data(st, ptr, Str::mod_, vec![lhs, rhs]),
    ast::BinaryOpKind::Plus => bop(BinaryOp::Add, lhs, rhs),
    ast::BinaryOpKind::Minus => bop(BinaryOp::Sub, lhs, rhs),
    ast::BinaryOpKind::LtLt => bop(BinaryOp::Shl, lhs, rhs),
    ast::BinaryOpKind::GtGt => bop(BinaryOp::Shr, lhs, rhs),
    ast::BinaryOpKind::Lt => bop(BinaryOp::Lt, lhs, rhs),
    ast::BinaryOpKind::LtEq => bop(BinaryOp::LtEq, lhs, rhs),
    ast::BinaryOpKind::Gt => bop(BinaryOp::Gt, lhs, rhs),
    ast::BinaryOpKind::GtEq => bop(BinaryOp::GtEq, lhs, rhs),
    ast::BinaryOpKind::EqEq => call_std_func_data(st, ptr, Str::equals, vec![lhs, rhs]),
    ast::BinaryOpKind::BangEq => {
      let inner = call_std_func(st, ptr, Str::equals, vec![lhs, rhs]);
      ExprData::UnaryOp { op: UnaryOp::LogicalNot, inner }
    }
    ast::BinaryOpKind::InKw => call_std_func_data(st, ptr, Str::objectHasEx, vec![lhs, rhs]),
    ast::BinaryOpKind::And => bop(BinaryOp::BitAnd, lhs, rhs),
    ast::BinaryOpKind::Carat => bop(BinaryOp::BitXor, lhs, rhs),
    ast::BinaryOpKind::Bar => bop(BinaryOp::BitOr, lhs, rhs),
    ast::BinaryOpKind::AndAnd => {
      let no = Some(st.expr(ptr, ExprData::Prim(Prim::Bool(false))));
      ExprData::If { cond: lhs, yes: rhs, no }
    }
    ast::BinaryOpKind::BarBar => {
      let yes = Some(st.expr(ptr, ExprData::Prim(Prim::Bool(true))));
      ExprData::If { cond: lhs, yes, no: rhs }
    }
  }
}
