use anyhow::{anyhow, Result};
use std::ops::ControlFlow;

pub(crate) fn extract_error<T, C>(e: lsp_server::ExtractError<C>) -> ControlFlow<Result<T>, C> {
  match e {
    lsp_server::ExtractError::MethodMismatch(x) => ControlFlow::Continue(x),
    lsp_server::ExtractError::JsonError { method, error } => {
      ControlFlow::Break(Err(anyhow!("couldn't deserialize for {method}: {error}")))
    }
  }
}
