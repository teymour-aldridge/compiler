//! The core of the implementation for the Language Protocol Server.
//!
//! todo: implement a custom rope for fun
//! todo: handle errors properly

use codespan_lsp::{byte_span_to_range, position_to_byte_index};
use codespan_reporting::{diagnostic::LabelStyle, files::Files};
use lsp_server::{Connection, Message};
use lsp_types::{
    notification::{DidChangeTextDocument, DidOpenTextDocument, Notification, PublishDiagnostics},
    DiagnosticRelatedInformation, Location, PublishDiagnosticsParams,
    TextDocumentContentChangeEvent,
};
use ropey::Rope;
use rustc_hash::FxHashMap;
use serde_json::to_value;

use crate::cast::cast_not;

#[derive(Default)]
pub(crate) struct FileContainer {
    inner: FxHashMap<lsp_types::Url, SingleFile>,
}

impl<'a> Files<'a> for FileContainer {
    type FileId = &'a lsp_types::Url;

    type Name = &'a lsp_types::Url;

    type Source = String;

    fn name(&'a self, id: Self::FileId) -> Result<Self::Name, codespan_reporting::files::Error> {
        self.inner
            .get_key_value(id)
            .map(|(a, _)| a)
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }

    fn source(
        &'a self,
        id: Self::FileId,
    ) -> Result<Self::Source, codespan_reporting::files::Error> {
        self.inner
            .get(id)
            .map(|x| x.to_string())
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }

    fn line_index(
        &'a self,
        id: Self::FileId,
        byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        self.inner
            .get(id)
            .map(|file| {
                // todo: this can be sped up
                // todo: don't panic if byte index is too big
                file.rope.byte_to_line(byte_index)
            })
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }

    fn line_range(
        &'a self,
        id: Self::FileId,
        line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        self.inner
            .get(id)
            .map(|file| {
                // todo: be more defensive
                let line = file.line(line_index);
                let start = file.line_to_byte(line_index);
                let end = start + line.bytes().len();
                start..end
            })
            .ok_or(codespan_reporting::files::Error::FileMissing)
    }
}

impl FileContainer {
    /// Processes the provided notification, and if applicable sends a response using the
    /// [lsp_server::Connection].
    pub(crate) fn handle_notification(&mut self, not: lsp_server::Notification, conn: &Connection) {
        let not = match cast_not::<DidOpenTextDocument>(not) {
            Ok(params) => {
                let path = params.text_document.uri.path();
                if path.ends_with(".pseudo") {
                    self.inner.insert(
                        params.text_document.uri,
                        SingleFile::new(&params.text_document.text),
                    );
                    self.publish_diagnostics(conn);
                }
                return;
            }
            Err(not) => not,
        };

        match cast_not::<DidChangeTextDocument>(not) {
            Ok(change) => {
                let path = change.text_document.uri.path();
                if !path.ends_with(".pseudo") {
                    return;
                }

                if self.inner.contains_key(&change.text_document.uri) {
                    self.apply_edits_to_file(&change.text_document.uri, &change.content_changes);
                } else {
                    self.inner
                        .insert(change.text_document.uri.clone(), SingleFile::new(""));
                    self.apply_edits_to_file(&change.text_document.uri, &change.content_changes);
                }
                self.publish_diagnostics(conn);
            }
            Err(_) => (),
        };
    }

    /// Applies the edits to a file.
    ///
    /// note: this function will panic if the file does not exist!
    /// todo: return an error if the file cannot be found
    fn apply_edits_to_file(
        &mut self,
        id: &lsp_types::Url,
        changes: &Vec<TextDocumentContentChangeEvent>,
    ) {
        enum Op {
            Replace((usize, usize, String)),
            Clear(String),
        }

        let mut edits = vec![];

        for change in changes {
            if let Some(range) = change.range {
                let (start, end) = (range.start, range.end);
                let (start, end) = (
                    position_to_byte_index(self, id, &start).unwrap(),
                    position_to_byte_index(self, id, &end).unwrap(),
                );
                edits.push(Op::Replace((start, end, change.text.clone())))
            } else {
                edits.push(Op::Clear(change.text.clone()))
            }
        }

        let file = self.inner.get_mut(id).unwrap();

        for each in edits {
            match each {
                Op::Replace((start, end, text)) => {
                    file.rope.remove(start..end);
                    file.rope.insert(start, &text);
                }
                Op::Clear(text) => {
                    file.rope.remove(..);
                    file.rope.insert(0, &text);
                }
            }
        }
    }

    /// Runs the compiler on the source files and sends the diagnostics back to the editor.
    ///
    /// todo: incremental compilation
    fn publish_diagnostics(&self, conn: &Connection) {
        for (url, file) in &self.inner {
            // todo: generate multiple errors
            let mut errors = vec![];

            let input = file.rope.to_string();

            let res = logic::parse::parse(&input)
                .map_err(|err| err.report(&url))
                .and_then(|tagged_ast| {
                    logic::ty::type_check(&tagged_ast)
                        .map_err(|e| e.report(&url, &tagged_ast))
                        .map(|_| ())
                });

            match res {
                Ok(()) => return,
                Err(e) => errors.push(e),
            }

            let diagnostics = errors
                .iter()
                .map(|diagnostic| -> lsp_types::Diagnostic {
                    let primary_label = diagnostic
                        .labels
                        .iter()
                        .find(|label| label.style == LabelStyle::Primary)
                        .expect("internal error - malshaped diagnostic (please report)");

                    let primary_label_range = primary_label.range.clone();
                    // todo: robust error handling
                    let primary_label_range =
                        byte_span_to_range(self, url, primary_label_range).unwrap();

                    let further_information = diagnostic
                        .labels
                        .iter()
                        .map(|label| {
                            let range = label.range.clone();
                            // todo: robust error handling
                            let range = byte_span_to_range(self, url, range).unwrap();
                            DiagnosticRelatedInformation {
                                location: Location {
                                    uri: url.clone(),
                                    range,
                                },
                                message: label.message.clone(),
                            }
                        })
                        .collect();

                    lsp_types::Diagnostic {
                        range: primary_label_range,
                        message: primary_label.message.clone(),
                        related_information: Some(further_information),
                        ..Default::default()
                    }
                })
                .collect();

            conn.sender
                .send(Message::Notification(lsp_server::Notification {
                    method: PublishDiagnostics::METHOD.to_string(),
                    // todo: be more defensive
                    params: to_value(PublishDiagnosticsParams {
                        uri: url.clone(),
                        diagnostics,
                        version: None,
                    })
                    .unwrap(),
                }))
                .expect("failed to send messages");
        }
    }
}

#[derive(Default)]
pub(crate) struct SingleFile {
    rope: Rope,
}

impl std::ops::Deref for SingleFile {
    type Target = Rope;

    fn deref(&self) -> &Self::Target {
        &self.rope
    }
}

impl SingleFile {
    fn new(text: &str) -> Self {
        Self {
            rope: Rope::from_str(text),
        }
    }
}
