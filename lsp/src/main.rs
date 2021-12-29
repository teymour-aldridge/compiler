//! An implementation of the language server protocol for OCR exam reference language.
//!
//! todo: publish this as a VSCode extension (ideally with weekly builds)

mod cast;
mod files;

use std::error::Error;

use files::FileContainer;
use lsp_server::Connection;
use lsp_types::{
    InitializeParams, ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
};
use serde_json::{from_value, to_value, Value};

type LspResult = Result<(), Box<dyn Error + Sync + Send>>;

fn main() -> LspResult {
    let (conn, io_threads) = Connection::stdio();

    let capabilities = to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::Incremental,
        )),
        ..Default::default()
    })
    .unwrap();

    let init_params = conn.initialize(capabilities)?;

    main_loop(conn, init_params)?;

    io_threads.join()?;

    Ok(())
}

fn main_loop(conn: Connection, params: Value) -> LspResult {
    let _: InitializeParams = from_value(params).unwrap();

    let mut files = FileContainer::default();

    for msg in &conn.receiver {
        match msg {
            lsp_server::Message::Notification(not) => files.handle_notification(not, &conn),
            _ => {}
        }
    }

    Ok(())
}
