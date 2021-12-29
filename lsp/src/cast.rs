//! Attempts to cast the given LSP messages to the appropriate type.

use lsp_server::{Notification, Request, RequestId};
use serde::de::DeserializeOwned;

/// Try to cast the request into the requested form.
#[allow(unused)]
pub fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), Request>
where
    R: lsp_types::request::Request,
    R::Params: DeserializeOwned,
{
    req.extract(R::METHOD)
}

/// Try to cast the notification into the requested form.
pub fn cast_not<N>(not: Notification) -> Result<N::Params, Notification>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}
