use std::{thread, time::Duration};

use lsp_server::{Connection, Message, Notification};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidOpenTextDocument, Exit, Initialized,
        Notification as LspNotification,
    },
    request::{Initialize, Request, Shutdown},
    ClientCapabilities, DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams,
    InitializedParams, Position, Range, TextDocumentContentChangeEvent, WorkspaceFolder,
};
use serde_json::to_value;

use crate::run;

#[test]
/// A basic test to check that at least some thing work.
#[allow(deprecated)]
fn smoke_test_1() {
    let (server, client) = Connection::memory();

    let lsp_server = thread::spawn(|| run(server));

    client
        .sender
        .send(Message::Request(lsp_server::Request::new(
            1.into(),
            Initialize::METHOD.to_string(),
            to_value(InitializeParams {
                process_id: Some(thread::current().id().as_u64().get() as u32),
                root_path: None,
                root_uri: Some(lsp_types::Url::parse("file:///Users/pseudodemo").unwrap()),
                initialization_options: None,
                capabilities: ClientCapabilities::default(),
                trace: None,
                workspace_folders: Some(vec![WorkspaceFolder {
                    uri: lsp_types::Url::parse("file:///Users/pseudodemo").unwrap(),
                    name: "pseudodemo".to_string(),
                }]),
                client_info: None,
                locale: None,
            })
            .unwrap(),
        )))
        .expect("failed to initialize");

    let res = client
        .receiver
        .recv_timeout(Duration::from_secs(10))
        .expect("failed to receive response");
    match res {
        Message::Response(_) => {}
        msg => {
            panic!("invalid message: {:#?}", msg)
        }
    }

    client
        .sender
        .send(Message::Notification(Notification::new(
            Initialized::METHOD.to_string(),
            InitializedParams {},
        )))
        .expect("failed to send message");

    client
        .sender
        .send(Message::Notification(Notification::new(
            DidOpenTextDocument::METHOD.to_string(),
            to_value(DidOpenTextDocumentParams {
                text_document: lsp_types::TextDocumentItem {
                    uri: lsp_types::Url::parse("file:///Users/pseudodemo/main.pseudo").unwrap(),
                    language_id: "pseudo".to_string(),
                    version: 1,
                    text: "".to_string(),
                },
            })
            .unwrap(),
        )))
        .expect("failed to send message");

    client
        .sender
        .send(Message::Notification(Notification::new(
            DidChangeTextDocument::METHOD.to_string(),
            DidChangeTextDocumentParams {
                text_document: lsp_types::VersionedTextDocumentIdentifier {
                    uri: lsp_types::Url::parse("file:///Users/pseudodemo/main.pseudo").unwrap(),
                    version: 2,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    }),
                    range_length: Some(0),
                    text: "a".to_string(),
                }],
            },
        )))
        .expect("failed to send message");

    client
        .sender
        .send(Message::Notification(Notification::new(
            DidChangeTextDocument::METHOD.to_string(),
            DidChangeTextDocumentParams {
                text_document: lsp_types::VersionedTextDocumentIdentifier {
                    uri: lsp_types::Url::parse("file:///Users/pseudodemo/main.pseudo").unwrap(),
                    version: 3,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 0,
                            character: 1,
                        },
                        end: Position {
                            line: 0,
                            character: 1,
                        },
                    }),
                    range_length: Some(0),
                    text: " ".to_string(),
                }],
            },
        )))
        .expect("failed to send message");

    client
        .sender
        .send(Message::Notification(Notification::new(
            DidChangeTextDocument::METHOD.to_string(),
            DidChangeTextDocumentParams {
                text_document: lsp_types::VersionedTextDocumentIdentifier {
                    uri: lsp_types::Url::parse("file:///Users/pseudodemo/main.pseudo").unwrap(),
                    version: 3,
                },
                content_changes: vec![TextDocumentContentChangeEvent {
                    range: Some(Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    }),
                    range_length: Some(0),
                    text: "*".to_string(),
                }],
            },
        )))
        .expect("failed to send message");

    client
        .sender
        .send(Message::Request(lsp_server::Request::new(
            2.into(),
            Shutdown::METHOD.to_string(),
            to_value(()).unwrap(),
        )))
        .unwrap();

    client
        .sender
        .send(Message::Notification(Notification::new(
            Exit::METHOD.to_string(),
            to_value(()).unwrap(),
        )))
        .unwrap();

    assert!(lsp_server.join().is_ok());
}
