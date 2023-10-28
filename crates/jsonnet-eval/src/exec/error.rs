#[derive(Debug)]
pub struct Error {
  pub expr: jsonnet_expr::ExprMust,
  pub kind: Kind,
}

#[derive(Debug)]
pub enum Kind {
  Todo(&'static str),
  ArrayIdxNotInteger,
  ArrayIdxOutOfRange,
  DuplicateArgument,
  DuplicateField,
  IncompatibleTypes,
  NoSuchArgument,
  NoSuchFieldName,
  TooManyArguments,
  Infinite(jsonnet_expr::Infinite),
  User(jsonnet_expr::Str),
}
