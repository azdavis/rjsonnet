//! Conversions between common types.

use always::always;
use anyhow::{Result, bail};
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

pub(crate) fn clean_path_buf(url: &Url) -> Result<paths::CleanPathBuf> {
  let pb = path_buf(url)?;
  match paths::CleanPathBuf::new(pb.as_path()) {
    Some(x) => Ok(x),
    None => bail!("not absolute: {}", pb.display()),
  }
}

pub(crate) fn url(path: &paths::CleanPath) -> Option<Url> {
  // we want a compile error if the Err type stops being ().
  if let Ok(x) = Url::from_file_path(path.as_path()) {
    Some(x)
  } else {
    always!(false, "canonical path to url error");
    None
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
  let severity = match d.severity {
    diagnostic::Severity::Warning => lsp_types::DiagnosticSeverity::WARNING,
    diagnostic::Severity::Error => lsp_types::DiagnosticSeverity::ERROR,
  };
  lsp_types::Diagnostic {
    range: lsp_range(d.range),
    severity: Some(severity),
    code: None,
    code_description: None,
    source: Some("rjsonnet".to_owned()),
    message: d.message,
    related_information: None,
    tags: None,
    data: None,
  }
}

pub(crate) fn lsp_range(r: text_pos::RangeUtf16) -> lsp_types::Range {
  lsp_types::Range { start: lsp_position(r.start), end: lsp_position(r.end) }
}

pub(crate) fn lsp_position(p: text_pos::PositionUtf16) -> lsp_types::Position {
  lsp_types::Position { line: p.line, character: p.col }
}

pub(crate) fn text_pos_range(r: lsp_types::Range) -> text_pos::RangeUtf16 {
  text_pos::RangeUtf16 { start: text_pos_position(r.start), end: text_pos_position(r.end) }
}

pub(crate) fn text_pos_position(p: lsp_types::Position) -> text_pos::PositionUtf16 {
  text_pos::PositionUtf16 { line: p.line, col: p.character }
}

pub(crate) fn completion(c: lang_srv_state::CompletionItem) -> lsp_types::CompletionItem {
  let kind = match c.kind {
    lang_srv_state::CompletionItemKind::Field => lsp_types::CompletionItemKind::FIELD,
  };
  lsp_types::CompletionItem {
    label: c.name,
    label_details: None,
    kind: Some(kind),
    detail: Some(c.ty),
    documentation: c.doc.map(|value| {
      lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
        kind: lsp_types::MarkupKind::Markdown,
        value,
      })
    }),
    deprecated: None,
    preselect: None,
    sort_text: None,
    filter_text: None,
    insert_text: None,
    insert_text_format: None,
    insert_text_mode: None,
    text_edit: c.text_edit.map(|x| lsp_types::CompletionTextEdit::Edit(text_edit(x))),
    additional_text_edits: c
      .additional_text_edits
      .map(|xs| xs.into_iter().map(text_edit).collect()),
    command: None,
    commit_characters: None,
    data: None,
    tags: None,
  }
}

fn text_edit(e: lang_srv_state::TextEdit) -> lsp_types::TextEdit {
  lsp_types::TextEdit { range: lsp_range(e.range), new_text: e.text }
}

pub(crate) fn text_doc_position(
  p: &lsp_types::TextDocumentPositionParams,
) -> Result<(paths::CleanPathBuf, text_pos::PositionUtf16)> {
  let path = clean_path_buf(&p.text_document.uri)?;
  let pos = text_pos_position(p.position);
  Ok((path, pos))
}
