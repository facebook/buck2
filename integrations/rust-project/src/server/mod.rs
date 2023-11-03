/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::path::PathBuf;
use std::time::Instant;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use lsp_server::Connection;
use lsp_server::IoThreads;
use lsp_server::ReqQueue;
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

pub struct State {
    server: Server,
    io_threads: IoThreads,
}

impl State {
    pub fn new(
        handle: Handle<Vec<Box<dyn Layer<Registry> + Send + Sync + 'static>>, Registry>,
    ) -> Result<Self, anyhow::Error> {
        let (connection, io_threads) = Connection::stdio();

        // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
        let capabilities = ServerCapabilities {
            ..Default::default()
        };

        let capabilities = serde_json::to_value(capabilities)?;
        let _initialization_params = connection.initialize(capabilities)?;

        let (sender, receiver) = (connection.sender, connection.receiver);
        let server = Server {
            sender: sender.clone(),
            receiver,
            req_queue: ReqQueue::default(),
        };

        handle
            .modify(|layers| {
                let progress = ProgressLayer::new(sender);
                layers.push(progress.boxed())
            })
            .expect("Unable to update subscriber");

        let state = State { server, io_threads };
        Ok(state)
    }

    pub fn run(self) -> Result<(), anyhow::Error> {
        let State {
            mut server,
            io_threads,
        } = self;

        register_did_save_capability(&mut server);

        info!("waiting for incoming messages");

        let receiver = server.receiver.clone();
        for msg in receiver {
            match msg {
                lsp_server::Message::Request(req) => {
                    let mut dispatcher = Dispatcher {
                        req: Some(req),
                        server: &mut server,
                    };

                    dispatcher
                        .on::<DiscoverBuckTargets>(handle_discover_buck_targets)
                        .on::<lsp_types::request::Shutdown>(|_, ()| {
                            std::process::exit(0);
                        })
                        .finish();
                }
                lsp_server::Message::Response(resp) => {
                    tracing::info!(resp = ?resp, "response");
                }
                lsp_server::Message::Notification(notification) => {
                    tracing::info!(notification = ?notification, "notification");
                }
            }
        }
        io_threads.join()?;
        Ok(())
    }
}

pub struct Dispatcher<'a> {
    pub(crate) req: Option<lsp_server::Request>,
    pub(crate) server: &'a mut Server,
}

impl<'a> Dispatcher<'a> {
    pub(crate) fn on<R>(
        &mut self,
        f: fn(&mut Server, R::Params) -> anyhow::Result<R::Result>,
    ) -> &mut Self
    where
        R: lsp_types::request::Request + 'static,
        R::Params: for<'de> Deserialize<'de> + fmt::Debug,
        R::Result: Serialize,
    {
        let (req, params) = match self.parse::<R>() {
            Some(it) => it,
            None => return self,
        };

        info!(id = ?req.id, ?params, "received request");
        self.server
            .register_request(&req, std::time::Instant::now());

        let result = { f(self.server, params) };
        if let Ok(response) = result_to_response::<R>(req.id, result) {
            self.server.respond(response);
        }

        self
    }

    pub(crate) fn finish(&mut self) {
        if let Some(req) = self.req.take() {
            tracing::error!("unknown request: {:?}", req);
            let response = lsp_server::Response::new_err(
                req.id,
                lsp_server::ErrorCode::MethodNotFound as i32,
                "unknown request".to_owned(),
            );
            self.server.respond(response);
        }
    }

    fn parse<R>(&mut self) -> Option<(lsp_server::Request, R::Params)>
    where
        R: lsp_types::request::Request,
        R::Params: for<'de> Deserialize<'de> + fmt::Debug,
    {
        let req = match &self.req {
            Some(req) if req.method == R::METHOD => self.req.take()?,
            _ => return None,
        };

        let res = serde_json::from_value(req.params.clone());
        match res {
            Ok(params) => Some((req, params)),
            Err(err) => {
                let response = lsp_server::Response::new_err(
                    req.id,
                    lsp_server::ErrorCode::InvalidParams as i32,
                    err.to_string(),
                );
                self.server.respond(response);
                None
            }
        }
    }
}

pub(crate) struct Server {
    sender: Sender<lsp_server::Message>,
    receiver: Receiver<lsp_server::Message>,
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

fn handle_discover_buck_targets(
    server: &mut Server,
    params: DiscoverBuckTargetParams,
) -> Result<JsonProject, anyhow::Error> {
    let token: lsp_types::NumberOrString =
        lsp_types::ProgressToken::String("rust-project/discoverBuckTargets".to_owned());

    // this request is load-bearing: it is necessary in order to start showing in-editor progress.
    server.send_request::<lsp_types::request::WorkDoneProgressCreate>(
        lsp_types::WorkDoneProgressCreateParams { token },
        |_, _| (),
    );
    let _guard =
        tracing::span!(target: "lsp_progress", tracing::Level::INFO, "resolving targets").entered();

    let project = Develop::new(params.text_documents).run()?;
    tracing::info!(crate_len = project.crates.len(), "created index");
    Ok(project)
}

fn register_did_save_capability(server: &mut Server) {
    info!("registering interest in save events");
    let save_registration_options = lsp_types::TextDocumentSaveRegistrationOptions {
        include_text: Some(false),
        text_document_registration_options: lsp_types::TextDocumentRegistrationOptions {
            document_selector: Some(vec![
                lsp_types::DocumentFilter {
                    language: None,
                    scheme: None,
                    pattern: Some("**/BUCK".into()),
                },
                lsp_types::DocumentFilter {
                    language: None,
                    scheme: None,
                    pattern: Some("**/TARGETS".into()),
                },
            ]),
        },
    };

    let registration = lsp_types::Registration {
        id: "textDocument/didSave".to_owned(),
        method: "textDocument/didSave".to_owned(),
        register_options: Some(serde_json::to_value(save_registration_options).unwrap()),
    };
    server.send_request::<lsp_types::request::RegisterCapability>(
        lsp_types::RegistrationParams {
            registrations: vec![registration],
        },
        |_, _| (),
    );
}

fn result_to_response<R>(
    id: lsp_server::RequestId,
    result: anyhow::Result<R::Result>,
) -> Result<lsp_server::Response, anyhow::Error>
where
    R: lsp_types::request::Request,
    R::Params: for<'de> Deserialize<'de>,
    R::Result: Serialize,
{
    let res = match result {
        Ok(resp) => lsp_server::Response::new_ok(id, &resp),
        Err(e) => lsp_server::Response::new_err(
            id,
            lsp_server::ErrorCode::InternalError as i32,
            e.to_string(),
        ),
    };

    Ok(res)
}

type ReqHandler = fn(&mut Server, lsp_server::Response);

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
