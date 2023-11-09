/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use std::time::Instant;

use crossbeam::channel::Receiver;
use crossbeam::channel::Sender;
use lsp_server::Connection;
use lsp_server::ExtractError;
use lsp_server::IoThreads;
use lsp_server::ReqQueue;
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use lsp_types::ServerCapabilities;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;
use tracing::warn;
use tracing_subscriber::reload::Handle;
use tracing_subscriber::Layer;
use tracing_subscriber::Registry;

use crate::cli::Develop;
use crate::cli::Input;
use crate::json_project::Crate;
use crate::json_project::JsonProject;
use crate::progress::ProgressLayer;
use crate::target::Target;

pub struct State {
    server: Server,
    projects: Vec<JsonProject>,
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

        let state = State {
            server,
            io_threads,
            projects: vec![],
        };
        Ok(state)
    }

    pub fn run(mut self) -> Result<(), anyhow::Error> {
        register_did_save_capability(&mut self.server);

        info!("waiting for incoming messages");
        let lsp_receiver = self.server.receiver.clone();
        for msg in lsp_receiver {
            self.handle_lsp(msg)?;
        }
        self.io_threads.join()?;
        Ok(())
    }

    fn handle_lsp(&mut self, msg: lsp_server::Message) -> Result<(), anyhow::Error> {
        match msg {
            lsp_server::Message::Request(req) => {
                let mut dispatcher = RequestDispatch {
                    req: Some(req),
                    state: self,
                };

                dispatcher
                    .on::<DiscoverBuckTargets>(handle_discover_buck_targets)
                    .on::<lsp_types::request::Shutdown>(|_, ()| {
                        std::process::exit(0);
                    })
                    .finish();

                Ok(())
            }
            lsp_server::Message::Notification(notification) => {
                tracing::info!(notification = ?notification, "notification");
                let mut dispatcher = NotificationDispatch {
                    not: Some(notification),
                    state: self,
                };

                dispatcher
                    .on::<lsp_types::notification::DidSaveTextDocument>(handle_did_save_buck_file)?
                    .finish();
                Ok(())
            }
            lsp_server::Message::Response(resp) => {
                tracing::info!(resp = ?resp, "response");
                Ok(())
            }
        }
    }
}

pub struct RequestDispatch<'a> {
    pub(crate) req: Option<lsp_server::Request>,
    pub(crate) state: &'a mut State,
}

impl<'a> RequestDispatch<'a> {
    pub(crate) fn on<R>(
        &mut self,
        f: fn(&mut State, R::Params) -> anyhow::Result<R::Result>,
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
        self.state
            .server
            .register_request(&req, std::time::Instant::now());

        let result = { f(self.state, params) };
        if let Ok(response) = result_to_response::<R>(req.id, result) {
            self.state.server.respond(response);
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
            self.state.server.respond(response);
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
                info!(id = ?response.id, err = err.to_string(), "Unable to respond");
                self.state.server.respond(response);
                None
            }
        }
    }
}

pub(crate) struct NotificationDispatch<'a> {
    pub(crate) not: Option<lsp_server::Notification>,
    pub(crate) state: &'a mut State,
}

impl NotificationDispatch<'_> {
    pub(crate) fn on<N>(
        &mut self,
        f: fn(&mut State, N::Params) -> anyhow::Result<()>,
    ) -> anyhow::Result<&mut Self>
    where
        N: lsp_types::notification::Notification,
        N::Params: for<'de> Deserialize<'de> + fmt::Debug + Send,
    {
        let not = match self.not.take() {
            Some(it) => it,
            None => return Ok(self),
        };
        let params = match not.extract::<N::Params>(N::METHOD) {
            Ok(it) => it,
            Err(ExtractError::JsonError { method, error }) => {
                let error: Box<dyn std::error::Error + Send + Sync + 'static> = Box::new(error);
                tracing::error!(?method, error = error.as_ref(), "invalid request");
                return Ok(self);
            }
            Err(ExtractError::MethodMismatch(not)) => {
                self.not = Some(not);
                return Ok(self);
            }
        };

        f(self.state, params)?;
        Ok(self)
    }

    pub(crate) fn finish(&mut self) {
        if let Some(not) = &self.not {
            if !not.method.starts_with("$/") {
                tracing::error!("unhandled notification: {:?}", not);
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
            tracing::info!(?method, ?response.id, ?duration);
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
    state: &mut State,
    params: DiscoverBuckTargetParams,
) -> Result<Vec<JsonProject>, anyhow::Error> {
    let State {
        server, projects, ..
    } = state;
    let develop = Develop::new(Input::Files(params.text_documents));

    // this request is load-bearing: it is necessary in order to start showing in-editor progress.
    let token = lsp_types::ProgressToken::String(DiscoverBuckTargets::METHOD.to_owned());
    server.send_request::<lsp_types::request::WorkDoneProgressCreate>(
        lsp_types::WorkDoneProgressCreateParams {
            token: token.clone(),
        },
        |_, _| (),
    );
    let targets = tracing::span!(
        target: "lsp_progress",
        tracing::Level::INFO,
        "resolving targets",
        token = DiscoverBuckTargets::METHOD.to_owned(),
    )
    .in_scope(|| develop.resolve_owners())?;

    let Some(target) = targets.last() else {
        anyhow::bail!("Could not find any targets.");
    };

    // this request is load-bearing: it is necessary in order to start showing in-editor progress.
    let token = lsp_types::ProgressToken::String(target.to_string());
    server.send_request::<lsp_types::request::WorkDoneProgressCreate>(
        lsp_types::WorkDoneProgressCreateParams {
            token: token.clone(),
        },
        |_, _| (),
    );
    let span = tracing::span!(
        target: "lsp_progress",
        tracing::Level::INFO,
        "resolving targets",
        token = target.to_string(),
        label = target.to_string(),
    );
    let _guard = span.entered();

    let project = develop.run(targets)?;
    tracing::info!(crate_len = &project.crates.len(), "created index");
    projects.push(project);

    Ok(projects.clone())
}

fn handle_did_save_buck_file(
    state: &mut State,
    params: lsp_types::DidSaveTextDocumentParams,
) -> Result<(), anyhow::Error> {
    let State {
        server, projects, ..
    } = state;

    let mut projects = projects.clone();

    let path = params
        .text_document
        .uri
        .to_file_path()
        .expect("unable to convert URI to file path; this is a bug");

    let Some((idx, crates)) = find_changed_crate(&path, &projects) else {
        info!(?params.text_document, "could not find build file for document");
        return Ok(());
    };

    let targets = crates
        .iter()
        .map(|krate| krate.buck_extensions.label.clone())
        .collect::<Vec<Target>>();

    info!(?params.text_document, crates = ?targets, "got document");

    let Some(target) = targets.last() else {
        anyhow::bail!("Could not find any targets.");
    };

    let token = lsp_types::ProgressToken::String(target.to_string());
    server.send_request::<lsp_types::request::WorkDoneProgressCreate>(
        lsp_types::WorkDoneProgressCreateParams {
            token: token.clone(),
        },
        |_, _| (),
    );
    let _guard = tracing::span!(
        target: "lsp_progress",
        tracing::Level::INFO,
        "resolving targets",
        token = target.to_string(),
        label = target.to_string(),
    )
    .entered();

    let develop = Develop::new(Input::Targets(targets.clone()));
    let project = match develop.run(targets) {
        Ok(project) => project,
        Err(e) => {
            warn!(error = ?e, "unable to load updated file");
            return Ok(());
        }
    };

    projects[idx] = project;

    let notification = lsp_server::Notification::new(
        UpdatedBuckTargets::METHOD.to_string(),
        UpdatedBuckTargetsParams { projects },
    );

    server.send(notification.into());

    Ok(())
}

fn find_changed_crate<'a>(
    changed_file: &Path,
    projects: &'a [JsonProject],
) -> Option<(usize, Vec<&'a Crate>)> {
    for (idx, project) in projects.iter().enumerate() {
        let impacted_crates = project
            .crates
            .iter()
            .filter(|krate| krate.buck_extensions.build_file == changed_file)
            .collect::<Vec<&Crate>>();
        if !impacted_crates.is_empty() {
            return Some((idx, impacted_crates));
        }
    }
    None
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

type ReqHandler = fn(&mut State, lsp_server::Response);

pub enum DiscoverBuckTargets {}

impl lsp_types::request::Request for DiscoverBuckTargets {
    type Params = DiscoverBuckTargetParams;
    type Result = Vec<JsonProject>;
    const METHOD: &'static str = "rust-project/discoverBuckTargets";
}

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct DiscoverBuckTargetParams {
    text_documents: Vec<PathBuf>,
}

pub enum UpdatedBuckTargets {}

impl lsp_types::notification::Notification for UpdatedBuckTargets {
    type Params = UpdatedBuckTargetsParams;
    const METHOD: &'static str = "rust-project/updatedBuckTargets";
}

#[derive(Debug, Eq, PartialEq, Clone, Default, Deserialize, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdatedBuckTargetsParams {
    projects: Vec<JsonProject>,
}
