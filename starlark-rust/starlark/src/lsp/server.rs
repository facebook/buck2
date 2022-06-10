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

//! Based on the reference lsp-server example at <https://github.com/rust-analyzer/lsp-server/blob/master/examples/goto_def.rs>.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::{Arc, RwLock},
};

use gazebo::prelude::*;
use lsp_server::{Connection, Message, Notification, Request, RequestId, Response, ResponseError};
use lsp_types::{
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, LogMessage,
        PublishDiagnostics,
    },
    request::GotoDefinition,
    DefinitionOptions, Diagnostic, DidChangeTextDocumentParams, DidCloseTextDocumentParams,
    DidOpenTextDocumentParams, GotoDefinitionParams, GotoDefinitionResponse, InitializeParams,
    Location, LogMessageParams, MessageType, OneOf, PublishDiagnosticsParams, ServerCapabilities,
    TextDocumentSyncCapability, TextDocumentSyncKind, Url, WorkDoneProgressOptions,
};
use serde::de::DeserializeOwned;

use crate::{analysis::DefinitionLocation, syntax::AstModule};

/// The result of evaluating a starlark program for use in the LSP.
pub struct LspEvalResult {
    /// The list of diagnostic issues that were encountered while evaluating a starlark program.
    pub diagnostics: Vec<Diagnostic>,
    /// If the program could be parsed, the parsed module.
    pub ast: Option<AstModule>,
}

/// Various pieces of context to allow the LSP to interact with starlark parsers, etc.
pub trait LspContext {
    /// Parse a file with the given contents. The filename is used in the diagnostics.
    fn parse_file_with_contents(&self, filename: &str, content: String) -> LspEvalResult;

    /// Resolve a path given in a `load()` statement.
    ///
    /// `path` is the string representation in the `load()` statement. Its meaning is
    ///        implementation defined.
    /// `current_file` is the the file that is including the `load()` statement, and should be used
    ///                if `path` is "relative" in a semantic sense.
    fn resolve_load(&self, path: &str, current_file: &Path) -> anyhow::Result<Url>;

    /// Get the contents of a starlark program at a given path, if it exists.
    fn get_load_contents(&self, uri: &Url) -> anyhow::Result<Option<String>>;

    /// Get the contents of a file at a given URI, and attempt to parse it.
    fn parse_file(&self, uri: &Url) -> anyhow::Result<Option<LspEvalResult>> {
        let result = self
            .get_load_contents(uri)?
            .map(|content| self.parse_file_with_contents(uri.path(), content));
        Ok(result)
    }
}

/// Errors when [`LspContext::resolve_load()`] cannot resolve a given path.
#[derive(thiserror::Error, Debug)]
pub enum ResolveLoadError {
    /// Attempted to resolve a relative path, but no current_file_path was provided,
    /// so it is not known what to resolve the path against.
    #[error("Relative path `{}` provided, but current_file_path could not be determined", .0.display())]
    MissingCurrentFilePath(PathBuf),
}

/// Errors when loading contents of a starlark program.
#[derive(thiserror::Error, Debug)]
pub enum LoadContentsError {
    /// The provided Url was not absolute and it needs to be.
    #[error("Provided path `{}` was not absolute", .0.path())]
    NotAbsolute(Url),
}

struct Backend<T: LspContext> {
    connection: Connection,
    context: T,
    /// The `AstModule` from the last time that a file was opened / changed and parsed successfully.
    /// Entries are evicted when the file is closed.
    last_valid_parse: RwLock<HashMap<Url, Arc<AstModule>>>,
}

/// The logic implementations of stuff
impl<T: LspContext> Backend<T> {
    fn server_capabilities() -> ServerCapabilities {
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::Full)),
            definition_provider: Some(OneOf::Right(DefinitionOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            ..ServerCapabilities::default()
        }
    }

    fn get_ast(&self, uri: &Url) -> Option<Arc<AstModule>> {
        let last_valid_parse = self.last_valid_parse.read().unwrap();
        last_valid_parse.get(uri).duped()
    }

    fn get_ast_or_load_from_disk(&self, uri: &Url) -> anyhow::Result<Option<Arc<AstModule>>> {
        let module = match self.get_ast(uri) {
            Some(result) => Some(result),
            None => self
                .context
                .parse_file(uri)?
                .and_then(|eval_result| eval_result.ast.map(Arc::new)),
        };
        Ok(module)
    }

    fn validate(&self, uri: Url, version: Option<i64>, text: String) {
        let eval_result = self.context.parse_file_with_contents(uri.as_ref(), text);
        if let Some(ast) = eval_result.ast {
            let ast = Arc::new(ast);
            let mut last_valid_parse = self.last_valid_parse.write().unwrap();
            last_valid_parse.insert(uri.clone(), ast);
        }
        self.publish_diagnostics(uri, eval_result.diagnostics, version)
    }

    fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.validate(
            params.text_document.uri,
            Some(params.text_document.version as i64),
            params.text_document.text,
        )
    }

    fn did_change(&self, params: DidChangeTextDocumentParams) {
        // We asked for Sync full, so can just grab all the text from params
        let change = params.content_changes.into_iter().next().unwrap();
        self.validate(
            params.text_document.uri,
            Some(params.text_document.version as i64),
            change.text,
        );
    }

    fn did_close(&self, params: DidCloseTextDocumentParams) {
        {
            let mut last_valid_parse = self.last_valid_parse.write().unwrap();
            last_valid_parse.remove(&params.text_document.uri);
        }
        self.publish_diagnostics(params.text_document.uri, Vec::new(), None)
    }

    /// Go to the definition of the symbol at the current cursor if that definition is in
    /// the same file.
    ///
    /// NOTE: This uses the last valid parse of a file as a basis for symbol locations.
    /// If a file has changed and does result in a valid parse, then symbol locations may
    /// be slightly incorrect.
    fn goto_definition(&self, id: RequestId, params: GotoDefinitionParams) {
        self.send_response(new_response(id, self.find_definition(params)));
    }

    fn resolve_load_path(&self, path: &str, current_uri: &Url) -> anyhow::Result<Url> {
        let current_file = Path::new(current_uri.path());
        self.context.resolve_load(path, current_file)
    }

    fn find_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> anyhow::Result<GotoDefinitionResponse> {
        let uri = params.text_document_position_params.text_document.uri;
        let line = params.text_document_position_params.position.line;
        let character = params.text_document_position_params.position.character;

        let location = match self.get_ast(&uri) {
            Some(ast) => match ast.find_definition(line, character) {
                DefinitionLocation::Location(span) => Some(Location {
                    uri,
                    range: span.into(),
                }),
                DefinitionLocation::LoadedLocation {
                    location,
                    path,
                    name,
                } => {
                    let load_uri = self.resolve_load_path(&path, &uri)?;
                    let loaded_location = self
                        .get_ast_or_load_from_disk(&load_uri)?
                        .and_then(|ast| ast.find_exported_symbol(&name));
                    match loaded_location {
                        None => Some(Location {
                            uri,
                            range: location.into(),
                        }),
                        Some(loaded_location) => Some(Location {
                            uri: load_uri,
                            range: loaded_location.into(),
                        }),
                    }
                }
                DefinitionLocation::NotFound => None,
                DefinitionLocation::LoadPath { path } => {
                    match self.resolve_load_path(&path, &uri) {
                        Ok(load_uri) => Some(Location {
                            uri: load_uri,
                            range: Default::default(),
                        }),
                        Err(_) => None,
                    }
                }
            },
            None => None,
        };

        let response = match location {
            Some(location) => GotoDefinitionResponse::Scalar(location),
            None => GotoDefinitionResponse::Array(vec![]),
        };
        Ok(response)
    }
}

/// The library style pieces
impl<T: LspContext> Backend<T> {
    fn send_notification(&self, x: Notification) {
        self.connection
            .sender
            .send(Message::Notification(x))
            .unwrap()
    }

    fn send_response(&self, x: Response) {
        self.connection.sender.send(Message::Response(x)).unwrap()
    }

    fn log_message(&self, typ: MessageType, message: &str) {
        self.send_notification(new_notification::<LogMessage>(LogMessageParams {
            typ,
            message: message.to_owned(),
        }))
    }

    fn publish_diagnostics(&self, uri: Url, diags: Vec<Diagnostic>, version: Option<i64>) {
        self.send_notification(new_notification::<PublishDiagnostics>(
            PublishDiagnosticsParams::new(uri, diags, version.map(|i| i as i32)),
        ));
    }

    fn main_loop(&self, _params: InitializeParams) -> anyhow::Result<()> {
        self.log_message(MessageType::Info, "Starlark server initialised");
        for msg in &self.connection.receiver {
            match msg {
                Message::Request(req) => {
                    // TODO(nmj): Also implement DocumentSymbols so that some logic can
                    //            be handled client side.
                    if let Some(params) = as_request::<GotoDefinition>(&req) {
                        self.goto_definition(req.id, params);
                    } else if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    // Currently don't handle any other requests
                }
                Message::Notification(x) => {
                    if let Some(params) = as_notification::<DidOpenTextDocument>(&x) {
                        self.did_open(params)
                    } else if let Some(params) = as_notification::<DidChangeTextDocument>(&x) {
                        self.did_change(params)
                    } else if let Some(params) = as_notification::<DidCloseTextDocument>(&x) {
                        self.did_close(params)
                    }
                }
                Message::Response(_) => {
                    // Don't expect any of these
                }
            }
        }
        Ok(())
    }
}

/// Instantiate an LSP server that reads on stdin, and writes to stdout
pub fn stdio_server<T: LspContext>(context: T) -> anyhow::Result<()> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("Starting Rust Starlark server");

    let (connection, io_threads) = Connection::stdio();
    server_with_connection(connection, context)?;
    // Make sure that the io threads stop properly too.
    io_threads.join()?;

    eprintln!("Stopping Rust Starlark server");
    Ok(())
}

/// Instantiate an LSP server that reads and writes using the given connection.
pub fn server_with_connection<T: LspContext>(
    connection: Connection,
    context: T,
) -> anyhow::Result<()> {
    // Run the server and wait for the main thread to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&Backend::<T>::server_capabilities()).unwrap();
    let initialization_params = connection.initialize(server_capabilities)?;
    let initialization_params = serde_json::from_value(initialization_params).unwrap();
    Backend {
        connection,
        context,
        last_valid_parse: RwLock::default(),
    }
    .main_loop(initialization_params)?;

    Ok(())
}

fn as_notification<T>(x: &Notification) -> Option<T::Params>
where
    T: lsp_types::notification::Notification,
    T::Params: DeserializeOwned,
{
    if x.method == T::METHOD {
        let params = serde_json::from_value(x.params.clone()).unwrap_or_else(|err| {
            panic!(
                "Invalid notification\nMethod: {}\n error: {}",
                x.method, err
            )
        });
        Some(params)
    } else {
        None
    }
}

fn as_request<T>(x: &Request) -> Option<T::Params>
where
    T: lsp_types::request::Request,
    T::Params: DeserializeOwned,
{
    if x.method == T::METHOD {
        let params = serde_json::from_value(x.params.clone())
            .unwrap_or_else(|err| panic!("Invalid request\nMethod: {}\n error: {}", x.method, err));
        Some(params)
    } else {
        None
    }
}

/// Create a new `Notification` object with the correct name from the given params.
pub(crate) fn new_notification<T>(params: T::Params) -> Notification
where
    T: lsp_types::notification::Notification,
{
    Notification {
        method: T::METHOD.to_owned(),
        params: serde_json::to_value(&params).unwrap(),
    }
}

fn new_response<T>(id: RequestId, params: anyhow::Result<T>) -> Response
where
    T: serde::Serialize,
{
    match params {
        Ok(params) => Response {
            id,
            result: Some(serde_json::to_value(params).unwrap()),
            error: None,
        },
        Err(e) => Response {
            id,
            result: None,
            error: Some(ResponseError {
                code: 0,
                message: format!("{:#?}", e),
                data: None,
            }),
        },
    }
}

// TODO(nmj): Some of the windows tests get a bit flaky, especially around
//            some paths. Revisit later.
#[cfg(all(test, not(windows)))]
mod test {
    use std::path::{Path, PathBuf};

    use lsp_server::{Request, RequestId};
    use lsp_types::{
        request::GotoDefinition, GotoDefinitionParams, GotoDefinitionResponse, Location, Position,
        Range, TextDocumentIdentifier, TextDocumentPositionParams, Url,
    };
    use textwrap::dedent;

    use crate::{analysis::FixtureWithRanges, lsp::test::TestServer};

    fn goto_definition_request(
        server: &mut TestServer,
        uri: Url,
        line: u32,
        character: u32,
    ) -> Request {
        server.new_request::<GotoDefinition>(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position { line, character },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        })
    }

    fn goto_definition_response_location(
        server: &mut TestServer,
        request_id: RequestId,
    ) -> anyhow::Result<Location> {
        let response = server.get_response::<GotoDefinitionResponse>(request_id)?;
        match response {
            GotoDefinitionResponse::Scalar(location) => Ok(location),
            _ => Err(anyhow::anyhow!("Got invalid message type: {:?}", response)),
        }
    }

    #[cfg(windows)]
    fn temp_file_uri(rel_path: &str) -> Url {
        Url::from_file_path(&PathBuf::from("C:/tmp").join(rel_path)).unwrap()
    }

    #[cfg(not(windows))]
    fn temp_file_uri(rel_path: &str) -> Url {
        Url::from_file_path(&PathBuf::from("/tmp").join(rel_path)).unwrap()
    }

    #[test]
    fn sends_empty_goto_definition_on_nonexistent_file() -> anyhow::Result<()> {
        let mut server = TestServer::new()?;
        let req = goto_definition_request(&mut server, temp_file_uri("nonexistent"), 0, 0);

        let request_id = server.send_request(req)?;
        let response: GotoDefinitionResponse = server.get_response(request_id)?;
        match response {
            GotoDefinitionResponse::Array(definitions) if definitions.is_empty() => Ok(()),
            response => Err(anyhow::anyhow!(
                "Expected empty definitions, got `{:?}`",
                response
            )),
        }
    }

    #[test]
    fn sends_empty_goto_definition_on_non_access_symbol() -> anyhow::Result<()> {
        let uri = temp_file_uri("file.star");

        let mut server = TestServer::new()?;
        let contents = "y = 1\ndef nothing():\n    pass\nprint(nothing())\n";
        server.open_file(uri.clone(), contents.to_owned())?;

        let goto_definition = goto_definition_request(&mut server, uri, 1, 6);

        let request_id = server.send_request(goto_definition)?;
        let response = server.get_response::<GotoDefinitionResponse>(request_id)?;
        match response {
            GotoDefinitionResponse::Array(definitions) if definitions.is_empty() => Ok(()),
            response => Err(anyhow::anyhow!(
                "Expected empty definitions, got `{:?}`",
                response
            )),
        }
    }

    #[test]
    fn goes_to_definition() -> anyhow::Result<()> {
        let uri = temp_file_uri("file.star");
        let expected_location = Location {
            uri: uri.clone(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 4,
                },
                end: Position {
                    line: 1,
                    character: 11,
                },
            },
        };

        let mut server = TestServer::new()?;
        let contents = "y = 1\ndef nothing():\n    pass\nprint(nothing())\n";
        server.open_file(uri.clone(), contents.to_owned())?;

        let goto_definition = goto_definition_request(&mut server, uri, 3, 6);

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn returns_old_definitions_if_current_file_does_not_parse() -> anyhow::Result<()> {
        let uri = temp_file_uri("file.star");
        let expected_location = Location {
            uri: uri.clone(),
            range: Range {
                start: Position {
                    line: 1,
                    character: 4,
                },
                end: Position {
                    line: 1,
                    character: 11,
                },
            },
        };

        let mut server = TestServer::new()?;
        let contents = "y = 1\ndef nothing():\n    pass\nprint(nothing())\n";
        server.open_file(uri.clone(), contents.to_owned())?;
        server.change_file(uri.clone(), "\"invalid parse".to_owned())?;

        let goto_definition = goto_definition_request(&mut server, uri, 3, 6);

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn jumps_to_definition_from_opened_loaded_file() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("{load}", "baz")
            <baz>b</baz>az()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: bar_uri.clone(),
            range: bar.span("baz").into(),
        };

        let mut server = TestServer::new()?;
        // Initialize with "junk" on disk so that we make sure we're using the contents from the
        // client (potentially indicating an unsaved, modified file)
        server.set_file_contents(PathBuf::from(bar_uri.path()), "some_symbol = 1".to_owned())?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.open_file(bar_uri, bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("baz"),
            foo.begin_column("baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn jumps_to_definition_from_closed_loaded_file() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("{load}", "baz")
            <baz>b</baz>az()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: bar_uri.clone(),
            range: bar.span("baz").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("baz"),
            foo.begin_column("baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn passes_cwd_for_relative_loads() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("bar.star", "baz")
            <baz>b</baz>az()
            "#,
        )
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: bar_uri.clone(),
            range: bar.span("baz").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("baz"),
            foo.begin_column("baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn does_not_jump_to_definition_if_invalid_file() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");

        let foo_contents = dedent(
            r#"
            load("{load}", <baz_loc>"baz"</baz_loc>)
            <baz>b</baz>az()
            "#,
        )
        .replace("{load}", foo_uri.path())
        .trim()
        .to_owned();
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let expected_location = Location {
            uri: foo_uri.clone(),
            range: foo.span("baz_loc").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("baz"),
            foo.begin_column("baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn does_not_jump_to_definition_if_symbol_not_found() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("bar.star", <not_baz_loc>"not_baz"</not_baz_loc>)
            <not_baz>not_baz</not_baz>()
            "#,
        )
        .trim()
        .to_owned();
        let bar_contents = "def baz():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: foo_uri.clone(),
            range: foo.span("not_baz_loc").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("not_baz"),
            foo.begin_column("not_baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn jumps_to_definition_in_load_statement() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("{load}", "<baz>b</baz>az")
            baz()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: bar_uri.clone(),
            range: bar.span("baz").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("baz"),
            foo.begin_column("baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn does_not_jump_to_definition_in_load_statement_if_not_found() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("bar.star", <not_baz_loc>"no<not_baz>t</not_baz>_baz"</not_baz_loc>)
            not_baz()
            "#,
        )
        .trim()
        .to_owned();
        let bar_contents = "def baz():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = Location {
            uri: foo_uri.clone(),
            range: foo.span("not_baz_loc").into(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("not_baz"),
            foo.begin_column("not_baz"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);
        Ok(())
    }

    #[test]
    fn jumps_to_file_in_load_statement() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");
        let bar_load_string = Path::new(bar_uri.path())
            .parent()
            .unwrap()
            .join("<bar>b</bar>ar<dot>.</dot>star")
            .display()
            .to_string();

        let foo_contents = dedent(
            r#"
            load("{load}", "baz")
            baz()
            "#,
        )
        .replace("{load}", &bar_load_string)
        .trim()
        .to_owned();
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;

        let expected_location = Location {
            uri: bar_uri,
            range: Default::default(),
        };

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("bar"),
            foo.begin_column("bar"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("dot"),
            foo.begin_column("dot"),
        );

        let request_id = server.send_request(goto_definition)?;
        let location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_location, location);

        Ok(())
    }
}
