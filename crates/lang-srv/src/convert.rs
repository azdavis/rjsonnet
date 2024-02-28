//! Conversions between common types.

use anyhow::{bail, Result};
use lsp_types::Url;

pub(crate) fn path_buf(url: &Url) -> Result<std::path::PathBuf> {
  if url.scheme() != "file" {
    bail!("not a file url: {url}")
  }
  match url.to_file_path() {
    Ok(x) => Ok(x),
    Err(()) => bail!("couldn't make a URL into a file path: {url}"),
  }
}

pub(crate) fn registration<N>(options: serde_json::Value) -> lsp_types::Registration
where
  N: lsp_types::notification::Notification,
{
  lsp_types::Registration {
    id: N::METHOD.to_owned(),
    method: N::METHOD.to_owned(),
    register_options: Some(options),
  }
}

pub(crate) fn diagnostic(d: diagnostic::Diagnostic) -> lsp_types::Diagnostic {
  lsp_types::Diagnostic {
    range: lsp_range(d.range),
    severity: Some(lsp_types::DiagnosticSeverity::ERROR),
    code: None,
    code_description: None,
    source: None,
    message: d.message,
    related_information: None,
    tags: None,
    data: None,
  }
}

pub(crate) fn lsp_range(r: text_pos::RangeUtf16) -> lsp_types::Range {
  lsp_types::Range { start: lsp_position(r.start), end: lsp_position(r.end) }
}

fn lsp_position(p: text_pos::PositionUtf16) -> lsp_types::Position {
  lsp_types::Position { line: p.line, character: p.col }
}

pub(crate) fn text_pos_range(r: lsp_types::Range) -> text_pos::RangeUtf16 {
  text_pos::RangeUtf16 { start: text_pos_position(r.start), end: text_pos_position(r.end) }
}

pub(crate) fn text_pos_position(p: lsp_types::Position) -> text_pos::PositionUtf16 {
  text_pos::PositionUtf16 { line: p.line, col: p.character }
}
