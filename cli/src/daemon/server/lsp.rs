/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use cli_proto::*;
use events::dispatch::EventDispatcher;
use futures::channel::mpsc::UnboundedSender;
use futures::FutureExt;
use futures::SinkExt;
use futures::StreamExt;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_types::Url;
use starlark::lsp::server::server_with_connection;
use starlark::lsp::server::LspContext;
use starlark::lsp::server::LspEvalResult;
use tonic::Status;

use crate::daemon::server::ServerCommandContext;
use crate::daemon::server::StreamingRequestHandler;

struct BuckLspContext {}

impl LspContext for BuckLspContext {
    fn parse_file_with_contents(&self, _uri: &Url, _content: String) -> LspEvalResult {
        LspEvalResult {
            diagnostics: vec![],
            ast: None,
        }
    }

    fn resolve_load(&self, _path: &str, _current_file: &Path) -> anyhow::Result<Url> {
        Err(anyhow::anyhow!("unimplemented"))
    }

    fn get_load_contents(&self, _uri: &Url) -> anyhow::Result<Option<String>> {
        Err(anyhow::anyhow!("unimplemented"))
    }
}

/// Run an LSP server for a given client.
pub(crate) async fn run_lsp_server(
    ctx: ServerCommandContext,
    mut req: StreamingRequestHandler<LspRequest>,
) -> anyhow::Result<LspResponse> {
    // This gets a bit messy because the various frameworks don't quite work the same way.
    // - tonic has async streams (which we pull from with .message().await)
    // - The LSP server uses crossbeam channels, which are synchronous, so we need to run
    //   recv from that channel it its own thread.
    // - We then need an async aware channel that takes from the LSP recv thread, and sends
    //   to the client. It needs to be async aware so that we can use select! and process
    //   client and server communication at the same time
    //
    // We also start up a thread to run the actual LSP, and make sure that we close those
    // threads down when we either fail to send/recv on the client channel or the server one.
    //
    // tl;dr; This creates two threads, and a few plumbing channels to get the client sending
    //        receiving to/from the LSP server.

    let (send_to_server, server_receiver) = crossbeam_channel::unbounded();
    let (send_to_client, client_receiver) = crossbeam_channel::unbounded();
    let (events_from_server, mut events_to_client) = futures::channel::mpsc::unbounded();

    let connection = Connection {
        sender: send_to_client,
        receiver: server_receiver,
    };

    let recv_thread = std::thread::spawn(move || {
        let runtime = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap();
        runtime.block_on(recv_from_lsp(client_receiver, events_from_server))
    });

    let server_thread =
        std::thread::spawn(move || server_with_connection(connection, BuckLspContext {}));

    let res = loop {
        let message_handler_res = tokio::select! {
            m = req.message().fuse() => {
                handle_incoming_lsp_message(&send_to_server, m)
            },
            m = events_to_client.next() => {
                Ok(handle_outgoing_lsp_message(ctx.events(), m))
            },
        };
        match message_handler_res {
            Ok(Some(res)) => break Ok(res),
            Ok(None) => {}
            Err(e) => break Err(e),
        };
    };

    let _ignored = recv_thread.join();
    let _ignored = server_thread.join();
    res
}

/// Receive messages from the LSP's channel, and pass them to the client after encapsulating them.
///
/// This returns `Ok(())` when the other end of the connection disconnects.
async fn recv_from_lsp(
    to_client: crossbeam_channel::Receiver<Message>,
    mut event_sender: UnboundedSender<buck2_data::LspResult>,
) -> anyhow::Result<()> {
    loop {
        let msg = to_client.recv()?;

        let lsp_json = serde_json::to_string(&msg).unwrap();
        let res = buck2_data::LspResult { lsp_json };
        match event_sender.send(res).await {
            Ok(_) => {}
            Err(e) if e.is_disconnected() => break Ok(()),
            Err(e) => break Err(e.into()),
        }
    }
}

/// Unpacks and validates messages from the client, sending them to the LSP.
///
/// Returns:
///     - `Ok(None)` if the message could be passed to the LSP
///     - `Ok(Some(LspResponse))` if the message was actually an error or the LSP could not accept
///                               the provided message. The stream should be terminated after this.
///     - `Err` if a payload could not be decoded.
fn handle_incoming_lsp_message(
    from_client: &crossbeam_channel::Sender<Message>,
    message: Result<LspRequest, Status>,
) -> anyhow::Result<Option<LspResponse>> {
    if let Ok(m) = message {
        let message = serde_json::from_str(&m.lsp_json)?;
        if from_client.send(message).is_ok() {
            return Ok(None);
        }
    }
    Ok(Some(LspResponse {}))
}

/// Handles an outgoing message from the LSP
///
/// Returns:
///     - `None` if the message could be passed to the client / event dispatcher.
///     - `Some(LspResponse)` if there was no message. This happens when the server is done
///                           sending messages, and the stream should be disconnected.
fn handle_outgoing_lsp_message(
    events: &EventDispatcher,
    event: Option<buck2_data::LspResult>,
) -> Option<LspResponse> {
    match event {
        Some(event) => {
            events.instant_event(event);
            None
        }
        None => Some(LspResponse {}),
    }
}
