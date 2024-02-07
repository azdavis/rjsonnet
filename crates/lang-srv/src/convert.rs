use anyhow::{bail, Result};
use lsp_types::Url;

pub(crate) fn path_buf(url: &Url) -> Result<std::path::PathBuf> {
  if url.scheme() != "file" {
    bail!("not a file url: {url}")
  }
  match url.to_file_path() {
    Ok(pb) => Ok(pb),
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
    range: range(d.range),
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

fn range(r: text_pos::RangeUtf16) -> lsp_types::Range {
  lsp_types::Range { start: position(r.start), end: position(r.end) }
}

fn position(p: text_pos::PositionUtf16) -> lsp_types::Position {
  lsp_types::Position { line: p.line, character: p.col }
}
