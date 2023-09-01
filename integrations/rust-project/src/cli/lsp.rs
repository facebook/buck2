/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::PathBuf;
use std::time::Instant;

use crossbeam::channel::Sender;
use lsp_server::Connection;
use lsp_server::ExtractError;
use lsp_server::ReqQueue;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_types::OneOf;
use lsp_types::ServerCapabilities;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;
use tracing_subscriber::reload::Handle;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;

use crate::cli::Develop;
use crate::json_project::JsonProject;
use crate::progress::ProgressLayer;

pub struct Lsp;

impl Lsp {
    pub fn start(
        handle: Handle<Vec<Box<dyn Layer<Registry> + Send + Sync + 'static>>, Registry>,
    ) -> Result<(), anyhow::Error> {
        let (connection, io_threads) = Connection::stdio();

        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        let capabilities = ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            ..Default::default()
        };

        let capabilities = serde_json::to_value(capabilities).unwrap();
        let _initialization_params = connection.initialize(capabilities)?;

        let (sender, receiver) = (connection.sender, connection.receiver);
        let mut server = Server {
            sender: sender.clone(),
            req_queue: ReqQueue::default(),
        };

        handle
            .modify(|layers| {
                let progress = ProgressLayer::new(sender);
                layers.push(progress.boxed())
            })
            .expect("Unable to update subscriber");

        info!("waiting for incoming messages");

        for msg in &receiver {
            if let lsp_server::Message::Request(req) = &msg {
                match cast::<lsp_types::request::Shutdown>(req.clone()) {
                    Ok(_) => break,
                    Err(err @ ExtractError::JsonError { .. }) => {
                        tracing::error!(?err, "unable to deserialize message");
                    }
                    Err(ExtractError::MethodMismatch(req)) => {
                        tracing::error!(?req, "method was mismatched");
                    }
                }
            };

            handle_server(&mut server, msg)?;
        }

        io_threads.join()?;
        Ok(())
    }
}

fn handle_server(server: &mut Server, msg: lsp_server::Message) -> Result<(), anyhow::Error> {
    let token: lsp_types::NumberOrString =
        lsp_types::ProgressToken::String("rust-project/discoverBuckTargets".to_owned());

    match msg {
        lsp_server::Message::Request(req) => match cast::<DiscoverBuckTargets>(req.clone()) {
            Ok((id, params)) => {
                info!(?id, ?params, "received request");
                server.register_request(&req, std::time::Instant::now());

                // this request is load-bearing: it is necessary in order to start showing in-editor progress.
                server.send_request::<lsp_types::request::WorkDoneProgressCreate>(
                    lsp_types::WorkDoneProgressCreateParams { token },
                    |_, _| (),
                );
                let _guard = tracing::span!(target: "lsp_progress", tracing::Level::INFO, "resolving targets").entered();

                let project = Develop::new(params.text_documents).run()?;

                let result = serde_json::to_value(&project)?;
                let resp = Response {
                    id,
                    result: Some(result),
                    error: None,
                };

                tracing::info!(crate_len = project.crates.len(), "created index");

                server.respond(resp);
            }
            Err(err @ ExtractError::JsonError { .. }) => {
                tracing::error!(?err, "unable to deserialize message");
            }
            Err(ExtractError::MethodMismatch(req)) => {
                tracing::error!(?req, "method was mismatched");
            }
        },
        lsp_server::Message::Response(resp) => {
            tracing::info!(resp = ?resp, "notification");
        }
        lsp_server::Message::Notification(notification) => {
            tracing::info!(notification = ?notification, "notification");
        }
    }
    Ok(())
}

type ReqHandler = fn(&mut Server, lsp_server::Response);

pub(crate) struct Server {
    sender: Sender<lsp_server::Message>,
    req_queue: ReqQueue<(String, Instant), ReqHandler>,
}

impl Server {
    pub(crate) fn register_request(
        &mut self,
        request: &lsp_server::Request,
        request_received: Instant,
    ) {
        self.req_queue.incoming.register(
            request.id.clone(),
            (request.method.clone(), request_received),
        );
    }

    pub(crate) fn send_request<R>(&mut self, params: R::Params, handler: ReqHandler)
    where
        R: lsp_types::request::Request,
    {
        let request = self
            .req_queue
            .outgoing
            .register(R::METHOD.to_owned(), params, handler);
        self.send(request.into());
    }

    pub(crate) fn respond(&mut self, response: lsp_server::Response) {
        if let Some((method, start)) = self.req_queue.incoming.complete(response.id.clone()) {
            let duration = start.elapsed();
            tracing::info!(
                "handled {} - ({}) in {:0.2?}",
                method,
                response.id,
                duration
            );
            self.send(response.into());
        } else {
            tracing::error!("Unable to complete response");
        }
    }

    pub(crate) fn send(&self, message: lsp_server::Message) {
        self.sender.send(message).expect("unable to send message");
    }
}

#[derive(Debug)]
pub enum DiscoverBuckTargets {}

impl lsp_types::request::Request for DiscoverBuckTargets {
    type Params = DiscoverBuckTargetParams;
    type Result = JsonProject;
    const METHOD: &'static str = "rust-project/discoverBuckTargets";
}

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoverBuckTargetParams {
    text_documents: Vec<PathBuf>,
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
