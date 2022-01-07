//! An implementation of the language server protocol for OCR exam reference language.
//!
//! todo: publish this as a VSCode extension (ideally with weekly builds)

#![cfg_attr(test, feature(thread_id_value))]

pub(crate) mod cast;
mod files;
#[cfg(test)]
mod regressions;

use std::error::Error;

use cast::{cast_not, cast_req};
use files::FileContainer;
use lsp_server::{Connection, Message, Response};
use lsp_types::{
    notification::Exit, request::Shutdown, InitializeParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind,
};
use serde_json::{from_value, to_value, Value};

type LspResult = Result<(), Box<dyn Error + Sync + Send>>;

fn main() -> LspResult {
    let (conn, io_threads) = Connection::stdio();

    run(conn)?;

    io_threads.join().map_err(Into::into)
}

fn run(conn: Connection) -> LspResult {
    let capabilities = to_value(&ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::Incremental,
        )),
        ..Default::default()
    })
    .unwrap();

    let init_params = conn.initialize(capabilities)?;

    main_loop(conn, init_params)?;

    Ok(())
}

fn main_loop(conn: Connection, params: Value) -> LspResult {
    let _: InitializeParams = from_value(params).unwrap();

    let mut files = FileContainer::default();

    let mut shutdown_received = false;

    for msg in &conn.receiver {
        match msg {
            lsp_server::Message::Notification(not) => {
                let not = match cast_not::<Exit>(not) {
                    Ok(_) if shutdown_received => {
                        break;
                    }
                    Ok(_) => {
                        // send message
                        todo!()
                    }
                    Err(not) => not,
                };
                files.handle_notification(not, &conn)
            }
            lsp_server::Message::Request(req) => {
                if let Ok((id, ())) = cast_req::<Shutdown>(req) {
                    shutdown_received = true;
                    conn.sender
                        .send(Message::Response(Response::new_ok(
                            id,
                            to_value(()).unwrap(),
                        )))
                        .unwrap();
                }
            }
            _ => {}
        }
    }

    Ok(())
}
