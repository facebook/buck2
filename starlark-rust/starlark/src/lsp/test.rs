/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;
use std::time::Duration;

use gazebo::prelude::*;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_server::ResponseError;
use lsp_types::notification::DidChangeTextDocument;
use lsp_types::notification::DidOpenTextDocument;
use lsp_types::notification::Exit;
use lsp_types::notification::Initialized;
use lsp_types::notification::Notification;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::request::Initialize;
use lsp_types::request::Request;
use lsp_types::request::Shutdown;
use lsp_types::ClientCapabilities;
use lsp_types::DidChangeTextDocumentParams;
use lsp_types::DidOpenTextDocumentParams;
use lsp_types::GotoCapability;
use lsp_types::InitializeParams;
use lsp_types::InitializeResult;
use lsp_types::InitializedParams;
use lsp_types::Position;
use lsp_types::PublishDiagnosticsParams;
use lsp_types::Range;
use lsp_types::TextDocumentClientCapabilities;
use lsp_types::TextDocumentContentChangeEvent;
use lsp_types::TextDocumentItem;
use lsp_types::Url;
use lsp_types::VersionedTextDocumentIdentifier;
use serde::de::DeserializeOwned;

use crate::errors::EvalMessage;
use crate::lsp::server::new_notification;
use crate::lsp::server::server_with_connection;
use crate::lsp::server::LoadContentsError;
use crate::lsp::server::LspContext;
use crate::lsp::server::LspEvalResult;
use crate::lsp::server::LspServerSettings;
use crate::lsp::server::LspUrl;
use crate::lsp::server::ResolveLoadError;
use crate::lsp::server::StringLiteralResult;
use crate::syntax::AstModule;
use crate::syntax::Dialect;
use crate::values::docs::render_docs_as_code;
use crate::values::docs::Doc;
use crate::values::docs::DocItem;
use crate::values::docs::Function;
use crate::values::docs::Identifier;
use crate::values::docs::Location;

/// Get the path from a URL, trimming off things like the leading slash that gets
/// appended in some windows test environments.
#[cfg(windows)]
fn get_path_from_uri(uri: &str) -> PathBuf {
    PathBuf::from(uri.trim_start_match('/'))
}

#[cfg(not(windows))]
fn get_path_from_uri(uri: &str) -> PathBuf {
    PathBuf::from(uri)
}

#[derive(thiserror::Error, Debug)]
pub(crate) enum TestServerError {
    #[error("Attempted to set the contents of a file with a non-absolute path `{}`", .0.display())]
    SetFileNotAbsolute(PathBuf),
    /// The response came back, but was an error response, not a successful one.
    #[error("Response error: {0:?}")]
    ResponseError(ResponseError),
    #[error("Invalid response message for request {0}: {1:?}")]
    InvalidResponse(RequestId, Response),
    #[error("Client received a request (not response/notification) from the server: {0:?}")]
    ReceivedRequest(lsp_server::Request),
    #[error("Got a duplicate response for request ID {:?}: Existing: {:?}, New: {:?}", .new.id, .existing, .new)]
    DuplicateResponse { new: Response, existing: Response },
}

struct TestServerContext {
    file_contents: Arc<RwLock<HashMap<PathBuf, String>>>,
    dirs: Arc<RwLock<HashSet<PathBuf>>>,
    builtin_docs: Arc<HashMap<LspUrl, String>>,
    builtin_symbols: Arc<HashMap<String, LspUrl>>,
}

impl LspContext for TestServerContext {
    fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult {
        match uri {
            LspUrl::File(path) | LspUrl::Starlark(path) => {
                match AstModule::parse(&path.to_string_lossy(), content, &Dialect::Extended) {
                    Ok(ast) => {
                        let diagnostics = ast.lint(None).into_map(|l| EvalMessage::from(l).into());
                        LspEvalResult {
                            diagnostics,
                            ast: Some(ast),
                        }
                    }
                    Err(e) => {
                        let diagnostics = vec![EvalMessage::from_anyhow(path, &e).into()];
                        LspEvalResult {
                            diagnostics,
                            ast: None,
                        }
                    }
                }
            }
            _ => LspEvalResult::default(),
        }
    }

    fn resolve_load(&self, path: &str, current_file: &LspUrl) -> anyhow::Result<LspUrl> {
        let path = PathBuf::from(path);
        match current_file {
            LspUrl::File(current_file_path) => {
                let current_file_dir = current_file_path.parent();
                let absolute_path = match (current_file_dir, path.is_absolute()) {
                    (_, true) => Ok(path),
                    (Some(current_file_dir), false) => Ok(current_file_dir.join(&path)),
                    (None, false) => Err(ResolveLoadError::MissingCurrentFilePath(path)),
                }?;
                Ok(Url::from_file_path(absolute_path).unwrap().try_into()?)
            }
            _ => Err(
                ResolveLoadError::WrongScheme("file://".to_owned(), current_file.clone()).into(),
            ),
        }
    }

    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &LspUrl,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        let re = regex::Regex::new(r#"--(\d+):(\d+):(\d+):(\d+)$"#)?;
        let (literal, range) = match re.captures(literal) {
            Some(cap) => {
                let start_line = cap.get(1).unwrap().as_str().parse().unwrap();
                let start_col = cap.get(2).unwrap().as_str().parse().unwrap();
                let end_line = cap.get(3).unwrap().as_str().parse().unwrap();
                let end_col = cap.get(4).unwrap().as_str().parse().unwrap();
                let range = Range::new(
                    Position::new(start_line, start_col),
                    Position::new(end_line, end_col),
                );
                (
                    literal[0..cap.get(0).unwrap().start()].to_owned(),
                    Some(range),
                )
            }
            None => (literal.to_owned(), None),
        };
        self.resolve_load(&literal, current_file)
            .map(|url| match &url {
                LspUrl::File(u) => match u.extension() {
                    Some(e) if e == "star" => Some(StringLiteralResult {
                        url,
                        location_finder: Some(box move |_ast, _url| Ok(range)),
                    }),
                    _ => Some(StringLiteralResult {
                        url,
                        location_finder: None,
                    }),
                },
                _ => None,
            })
    }

    fn get_load_contents(&self, uri: &LspUrl) -> anyhow::Result<Option<String>> {
        match uri {
            LspUrl::File(u) => {
                let path = get_path_from_uri(&u.to_string_lossy());
                let is_dir = self.dirs.read().unwrap().contains(&path);
                match (path.is_absolute(), is_dir) {
                    (true, false) => Ok(self.file_contents.read().unwrap().get(&path).cloned()),
                    (true, true) => {
                        Err(std::io::Error::from(std::io::ErrorKind::IsADirectory).into())
                    }
                    (false, _) => Err(LoadContentsError::NotAbsolute(uri.clone()).into()),
                }
            }
            LspUrl::Starlark(_) => Ok(self.builtin_docs.get(uri).cloned()),
            _ => Ok(None),
        }
    }

    fn get_url_for_global_symbol(
        &self,
        _current_file: &LspUrl,
        symbol: &str,
    ) -> anyhow::Result<Option<LspUrl>> {
        Ok(self.builtin_symbols.get(symbol).cloned())
    }
}

/// A server for use in testing that provides helpers for sending requests, correlating
/// responses, and sending / receiving notifications
pub struct TestServer {
    /// The thread that's actually running the server
    server_thread: Option<std::thread::JoinHandle<()>>,
    client_connection: Connection,
    /// Incrementing counter to automatically generate request IDs when making a request
    req_counter: i32,
    /// Simple incrementing document version counter
    version_counter: i32,
    /// A mapping of the requests that have arrived -> the response. Stored here as
    /// these responses might be interleaved with notifications and the like.
    responses: HashMap<RequestId, Response>,
    /// An ordered queue of all of the notifications that have been received. Drained as
    /// notifications are processed.
    notifications: VecDeque<lsp_server::Notification>,
    /// How long to wait for messages to be received.
    recv_timeout: Duration,
    file_contents: Arc<RwLock<HashMap<PathBuf, String>>>,
    dirs: Arc<RwLock<HashSet<PathBuf>>>,
    /// If it's been received, the response payload for initialization.
    initialize_response: Option<InitializeResult>,
    /// Documentation for built in symbols.
    builtin_docs: Arc<HashMap<LspUrl, String>>,
}

impl Drop for TestServer {
    fn drop(&mut self) {
        // Need to send both a Shutdown request and an Exit notification in succession
        // so that lsp_server knows to shutdown correctly.
        let req = lsp_server::Request {
            id: self.next_request_id(),
            method: Shutdown::METHOD.to_owned(),
            params: Default::default(),
        };
        if let Err(e) = self.send_request(req) {
            eprintln!("Server was already shutdown: {}", e);
        } else {
            let notif = lsp_server::Notification {
                method: Exit::METHOD.to_owned(),
                params: Default::default(),
            };
            if let Err(e) = self.send_notification(notif) {
                eprintln!("Could not send Exit notification: {}", e);
            }
        }

        if let Some(server_thread) = self.server_thread.take() {
            if let Err(e) = server_thread.join() {
                eprintln!("test server did not join when being dropped: {:?}", e);
            }
        }
    }
}

impl TestServer {
    pub(crate) fn docs_as_code(&self, uri: &LspUrl) -> Option<String> {
        self.builtin_docs.get(uri).cloned()
    }

    /// A static set of "builtins" to use for testing
    fn testing_builtins(root: &Path) -> anyhow::Result<HashMap<LspUrl, Vec<Doc>>> {
        let prelude_path = root.join("dir/prelude.bzl");
        let ret = hashmap! {
            LspUrl::try_from(Url::parse("starlark:/native/builtin.bzl")?)? => vec![
                Doc {
                    id: Identifier {
                        name: "native_function1".to_owned(),
                        location: None,
                    },
                    item: DocItem::Function(Function::default()),
                    custom_attrs: Default::default(),
                },
                Doc {
                    id: Identifier {
                        name: "native_function2".to_owned(),
                        location: None,
                    },
                    item: DocItem::Function(Function::default()),
                    custom_attrs: Default::default(),
                },
            ],
            LspUrl::try_from(Url::from_file_path(prelude_path).unwrap())? => vec![
                Doc {
                    id: Identifier {
                        name: "prelude_function".to_owned(),
                        location: Some(Location {
                            path: "//dir/prelude.bzl".to_owned(),
                            position: None,
                        }),
                    },
                    item: DocItem::Function(Function::default()),
                    custom_attrs: Default::default(),
                },
            ]
        };
        Ok(ret)
    }

    /// Generate a new request ID
    fn next_request_id(&mut self) -> RequestId {
        self.req_counter += 1;
        RequestId::from(self.req_counter)
    }

    fn next_document_version(&mut self) -> i32 {
        self.version_counter += 1;
        self.version_counter
    }

    /// Create a new request object with an automatically generated request ID.
    pub fn new_request<T: Request>(&mut self, params: T::Params) -> lsp_server::Request {
        lsp_server::Request {
            id: self.next_request_id(),
            method: T::METHOD.to_owned(),
            params: serde_json::to_value(params).unwrap(),
        }
    }

    /// Create and start a new LSP server. This sends the initialization messages with the given,
    /// initialization payload and makes sure that when the server is dropped, the threads
    /// are attempted to be stopped.
    pub(crate) fn new_with_settings(settings: Option<LspServerSettings>) -> anyhow::Result<Self> {
        let (server_connection, client_connection) = Connection::memory();

        let builtin = Self::testing_builtins(&std::env::current_dir()?)?;
        let mut builtin_docs = HashMap::with_capacity(builtin.len());
        let mut builtin_symbols = HashMap::new();

        for (u, ds) in builtin {
            builtin_docs.insert(u.clone(), render_docs_as_code(&ds).join("\n\n"));
            for d in ds {
                builtin_symbols.insert(d.id.name, u.clone());
            }
        }

        let builtin_docs = Arc::new(builtin_docs);
        let builtin_symbols = Arc::new(builtin_symbols);

        let prelude_file_contents = builtin_docs
            .iter()
            .filter_map(|(u, d)| match u {
                LspUrl::File(p) => Some((p.clone(), d.clone())),
                _ => None,
            })
            .collect();
        let file_contents = Arc::new(RwLock::new(prelude_file_contents));
        let dirs = Arc::new(RwLock::new(HashSet::new()));
        let ctx = TestServerContext {
            file_contents: file_contents.dupe(),
            dirs: dirs.dupe(),
            builtin_docs: builtin_docs.dupe(),
            builtin_symbols,
        };

        let server_thread = std::thread::spawn(|| {
            if let Err(e) = server_with_connection(server_connection, ctx) {
                eprintln!("Stopped test server thread with error `{:?}`", e);
            }
        });

        let ret = Self {
            server_thread: Some(server_thread),
            client_connection,
            req_counter: 0,
            version_counter: 0,
            responses: Default::default(),
            notifications: Default::default(),
            recv_timeout: Duration::from_secs(2),
            file_contents,
            dirs,
            initialize_response: None,
            builtin_docs,
        };
        ret.initialize(settings)
    }

    /// Create and start a new LSP server. This sends the initialization messages, and makes
    /// sure that when the server is dropped, the threads are attempted to be stopped.
    pub(crate) fn new() -> anyhow::Result<Self> {
        Self::new_with_settings(None)
    }

    fn initialize(mut self, settings: Option<LspServerSettings>) -> anyhow::Result<Self> {
        let capabilities = ClientCapabilities {
            text_document: Some(TextDocumentClientCapabilities {
                definition: Some(GotoCapability {
                    dynamic_registration: Some(true),
                    link_support: Some(true),
                }),
                ..Default::default()
            }),
            ..Default::default()
        };

        let initialization_options = settings.map(|v| serde_json::to_value(v).unwrap());

        let init = InitializeParams {
            process_id: None,
            #[allow(deprecated)]
            root_path: None,
            root_uri: None,
            initialization_options,
            capabilities,
            trace: None,
            workspace_folders: None,
            client_info: None,
            locale: None,
        };

        let init_request = self.new_request::<Initialize>(init);
        let initialize_id = self.send_request(init_request)?;

        let initialize_response = Some(self.get_response::<InitializeResult>(initialize_id)?);

        self.send_notification(lsp_server::Notification {
            method: Initialized::METHOD.to_owned(),
            params: serde_json::to_value(InitializedParams {})?,
        })?;

        self.initialize_response = initialize_response;

        Ok(self)
    }

    pub fn initialization_result(&self) -> Option<InitializeResult> {
        self.initialize_response.clone()
    }

    /// Send a request to the server, and get back the ID from the original message.
    pub fn send_request(&self, req: lsp_server::Request) -> anyhow::Result<RequestId> {
        let id = req.id.clone();
        self.send(Message::Request(req))?;
        Ok(id)
    }

    /// Send a notification to the server.
    pub fn send_notification(&self, notification: lsp_server::Notification) -> anyhow::Result<()> {
        self.send(Message::Notification(notification))
    }

    fn send(&self, message: Message) -> anyhow::Result<()> {
        Ok(self.client_connection.sender.send(message)?)
    }

    /// Receive messages from the server until either the response for the given request ID
    /// has been seen, or until there are no more messages and the receive method times out.
    pub fn get_response<T: DeserializeOwned>(&mut self, id: RequestId) -> anyhow::Result<T> {
        loop {
            self.receive()?;

            match self.responses.get(&id) {
                Some(Response {
                    error: None,
                    result: Some(result),
                    ..
                }) => {
                    break Ok(serde_json::from_value::<T>(result.clone())?);
                }
                Some(Response {
                    error: Some(err),
                    result: None,
                    ..
                }) => {
                    break Err(TestServerError::ResponseError(err.clone()).into());
                }
                Some(msg) => {
                    break Err(TestServerError::InvalidResponse(id, msg.clone()).into());
                }
                None => {}
            }
        }
    }

    pub fn get_notification<T: Notification>(&mut self) -> anyhow::Result<T::Params> {
        for _ in 0..10 {
            self.receive()?;
            let notification = self
                .notifications
                .iter()
                .enumerate()
                .find_map(|(i, n)| if T::METHOD == n.method { Some(i) } else { None })
                .and_then(|i| self.notifications.remove(i));
            if let Some(notification) = notification {
                return Ok(serde_json::from_value(notification.params)?);
            }
        }
        Err(anyhow::anyhow!(
            "Did not get a notification of type `{}` in 10 retries",
            T::METHOD
        ))
    }

    /// Attempt to receive a message and either put it in the `responses` map if it's a
    /// response, or the notifications queue if it's a notification.
    ///
    /// Returns an error if an invalid message is received, or if no message is
    /// received within the timeout.
    fn receive(&mut self) -> anyhow::Result<()> {
        let message = self
            .client_connection
            .receiver
            .recv_timeout(self.recv_timeout)?;
        match message {
            Message::Request(req) => Err(TestServerError::ReceivedRequest(req).into()),
            Message::Response(response) => match self.responses.entry(response.id.clone()) {
                Entry::Occupied(existing) => Err(TestServerError::DuplicateResponse {
                    new: response,
                    existing: existing.get().clone(),
                }
                .into()),
                Entry::Vacant(entry) => {
                    entry.insert(response);
                    Ok(())
                }
            },
            Message::Notification(notification) => {
                self.notifications.push_back(notification);
                Ok(())
            }
        }
    }

    /// Send a notification saying that a file was opened with the given contents.
    pub fn open_file(
        &mut self,
        uri: Url,
        contents: String,
    ) -> anyhow::Result<PublishDiagnosticsParams> {
        let open_params = DidOpenTextDocumentParams {
            text_document: TextDocumentItem {
                uri: uri.clone(),
                language_id: String::new(),
                version: self.next_document_version(),
                text: contents,
            },
        };
        let open_notification = new_notification::<DidOpenTextDocument>(open_params);
        self.send_notification(open_notification)?;
        let notification = self.get_notification::<PublishDiagnostics>()?;
        if notification.uri != uri {
            Err(anyhow::anyhow!(
                "Got a diagnostics message for `{}`, but expected it for `{}`",
                notification.uri,
                uri
            ))
        } else {
            Ok(notification)
        }
    }

    /// Send a notification saying that a file was changed with the given contents.
    pub fn change_file(&mut self, uri: Url, contents: String) -> anyhow::Result<()> {
        let change_params = DidChangeTextDocumentParams {
            text_document: VersionedTextDocumentIdentifier {
                uri,
                version: self.next_document_version(),
            },
            content_changes: vec![TextDocumentContentChangeEvent {
                range: None,
                range_length: None,
                text: contents,
            }],
        };
        let change_notification = new_notification::<DidChangeTextDocument>(change_params);
        self.send_notification(change_notification)?;
        Ok(())
    }

    /// Set the file contents that `get_load_contents()` will return. The path must be absolute.
    pub fn set_file_contents(&self, path: PathBuf, contents: String) -> anyhow::Result<()> {
        let path = get_path_from_uri(&format!("{}", path.display()));
        if !path.is_absolute() {
            Err(TestServerError::SetFileNotAbsolute(path).into())
        } else {
            self.file_contents.write().unwrap().insert(path, contents);
            Ok(())
        }
    }

    /// Configure a path to be "a directory". This will return IsADirectory as an
    /// error from get_load_contents
    pub fn mkdir(&self, uri: Url) {
        self.dirs.write().unwrap().insert(PathBuf::from(uri.path()));
    }
}
