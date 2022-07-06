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

use std::collections::HashMap;
use std::fmt::Debug;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;

use derivative::Derivative;
use gazebo::prelude::*;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_server::Notification;
use lsp_server::Request;
use lsp_server::RequestId;
use lsp_server::Response;
use lsp_server::ResponseError;
use lsp_types::notification::DidChangeTextDocument;
use lsp_types::notification::DidCloseTextDocument;
use lsp_types::notification::DidOpenTextDocument;
use lsp_types::notification::LogMessage;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::request::GotoDefinition;
use lsp_types::DefinitionOptions;
use lsp_types::Diagnostic;
use lsp_types::DidChangeTextDocumentParams;
use lsp_types::DidCloseTextDocumentParams;
use lsp_types::DidOpenTextDocumentParams;
use lsp_types::GotoDefinitionParams;
use lsp_types::GotoDefinitionResponse;
use lsp_types::InitializeParams;
use lsp_types::LocationLink;
use lsp_types::LogMessageParams;
use lsp_types::MessageType;
use lsp_types::OneOf;
use lsp_types::PublishDiagnosticsParams;
use lsp_types::Range;
use lsp_types::ServerCapabilities;
use lsp_types::TextDocumentSyncCapability;
use lsp_types::TextDocumentSyncKind;
use lsp_types::Url;
use lsp_types::WorkDoneProgressOptions;
use serde::de::DeserializeOwned;

use crate::analysis::DefinitionLocation;
use crate::analysis::LspModule;
use crate::syntax::AstModule;

/// The result of resolving a StringLiteral when looking up a definition.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct StringLiteralResult {
    /// The path that a string literal resolves to.
    pub url: Url,
    /// A function that takes the AstModule at path specified by `url` and that same url, and
    /// allows resolving a location to jump to within the specific URL if desired.
    ///
    /// If `None`, then just jump to the URL. Do not attempt to load the file.
    #[derivative(Debug = "ignore")]
    pub location_finder: Option<Box<dyn FnOnce(&AstModule, &Url) -> anyhow::Result<Option<Range>>>>,
}

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
    fn parse_file_with_contents(&self, uri: &Url, content: String) -> LspEvalResult;

    /// Resolve a path given in a `load()` statement.
    ///
    /// `path` is the string representation in the `load()` statement. Its meaning is
    ///        implementation defined.
    /// `current_file` is the the file that is including the `load()` statement, and should be used
    ///                if `path` is "relative" in a semantic sense.
    fn resolve_load(&self, path: &str, current_file: &Path) -> anyhow::Result<Url>;

    /// Resolve a string literal into a Url and a function that specifies a locaction within that
    /// target file.
    ///
    /// This can be used for things like file paths in string literals, build targets, etc.
    ///
    /// `current_file` is the file that is currently being evaluated
    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &Path,
    ) -> anyhow::Result<Option<StringLiteralResult>>;

    /// Get the contents of a starlark program at a given path, if it exists.
    fn get_load_contents(&self, uri: &Url) -> anyhow::Result<Option<String>>;

    /// Get the contents of a file at a given URI, and attempt to parse it.
    fn parse_file(&self, uri: &Url) -> anyhow::Result<Option<LspEvalResult>> {
        let result = self
            .get_load_contents(uri)?
            .map(|content| self.parse_file_with_contents(uri, content));
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
    last_valid_parse: RwLock<HashMap<Url, Arc<LspModule>>>,
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

    fn get_ast(&self, uri: &Url) -> Option<Arc<LspModule>> {
        let last_valid_parse = self.last_valid_parse.read().unwrap();
        last_valid_parse.get(uri).duped()
    }

    fn get_ast_or_load_from_disk(&self, uri: &Url) -> anyhow::Result<Option<Arc<LspModule>>> {
        let module = match self.get_ast(uri) {
            Some(result) => Some(result),
            None => self
                .context
                .parse_file(uri)?
                .and_then(|eval_result| eval_result.ast.map(|ast| Arc::new(LspModule::new(ast)))),
        };
        Ok(module)
    }

    fn validate(&self, uri: Url, version: Option<i64>, text: String) {
        let eval_result = self.context.parse_file_with_contents(&uri, text);
        if let Some(ast) = eval_result.ast {
            let module = Arc::new(LspModule::new(ast));
            let mut last_valid_parse = self.last_valid_parse.write().unwrap();
            last_valid_parse.insert(uri.clone(), module);
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
                DefinitionLocation::Location {
                    source,
                    destination: target,
                } => Some(LocationLink {
                    origin_selection_range: Some(source.into()),
                    target_uri: uri,
                    target_range: target.into(),
                    target_selection_range: target.into(),
                }),
                DefinitionLocation::LoadedLocation {
                    source,
                    destination: location,
                    path,
                    name,
                } => {
                    let load_uri = self.resolve_load_path(&path, &uri)?;
                    let loaded_location = self
                        .get_ast_or_load_from_disk(&load_uri)?
                        .and_then(|ast| ast.find_exported_symbol(&name));
                    match loaded_location {
                        None => Some(LocationLink {
                            origin_selection_range: Some(source.into()),
                            target_uri: uri,
                            target_range: location.into(),
                            target_selection_range: location.into(),
                        }),
                        Some(loaded_location) => Some(LocationLink {
                            origin_selection_range: Some(source.into()),
                            target_uri: load_uri,
                            target_range: loaded_location.into(),
                            target_selection_range: loaded_location.into(),
                        }),
                    }
                }
                DefinitionLocation::NotFound => None,
                DefinitionLocation::LoadPath { source, path } => {
                    match self.resolve_load_path(&path, &uri) {
                        Ok(load_uri) => Some(LocationLink {
                            origin_selection_range: Some(source.into()),
                            target_uri: load_uri,
                            target_range: Default::default(),
                            target_selection_range: Default::default(),
                        }),
                        Err(_) => None,
                    }
                }
                DefinitionLocation::StringLiteral { source, literal } => {
                    let literal = self
                        .context
                        .resolve_string_literal(&literal, Path::new(uri.path()))?;
                    match literal {
                        Some(StringLiteralResult {
                            url,
                            location_finder: Some(location_finder),
                        }) => {
                            // If there's an error loading the file to parse it, at least
                            // try to get to the file.
                            let target_range = self
                                .get_ast_or_load_from_disk(&url)
                                .and_then(|ast| match ast {
                                    Some(module) => location_finder(&module.ast, &url),
                                    None => Ok(None),
                                })
                                .inspect_err(|e| {
                                    eprintln!("Error jumping to definition: {:#}", e);
                                })
                                .unwrap_or_default()
                                .unwrap_or_default();
                            Some(LocationLink {
                                origin_selection_range: Some(source.into()),
                                target_uri: url,
                                target_range,
                                target_selection_range: target_range,
                            })
                        }
                        Some(StringLiteralResult {
                            url,
                            location_finder: None,
                        }) => {
                            let target_range = Range::default();
                            Some(LocationLink {
                                origin_selection_range: Some(source.into()),
                                target_uri: url,
                                target_range,
                                target_selection_range: target_range,
                            })
                        }
                        _ => None,
                    }
                }
            },
            None => None,
        };

        let response = match location {
            Some(location) => vec![location],
            None => vec![],
        };
        Ok(GotoDefinitionResponse::Link(response))
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
    use std::path::Path;
    use std::path::PathBuf;

    use lsp_server::Request;
    use lsp_server::RequestId;
    use lsp_types::request::GotoDefinition;
    use lsp_types::GotoDefinitionParams;
    use lsp_types::GotoDefinitionResponse;
    use lsp_types::LocationLink;
    use lsp_types::Position;
    use lsp_types::Range;
    use lsp_types::TextDocumentIdentifier;
    use lsp_types::TextDocumentPositionParams;
    use lsp_types::Url;
    use textwrap::dedent;

    use crate::analysis::FixtureWithRanges;
    use crate::codemap::ResolvedSpan;
    use crate::lsp::test::TestServer;

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
    ) -> anyhow::Result<LocationLink> {
        let response = server.get_response::<GotoDefinitionResponse>(request_id)?;
        match response {
            GotoDefinitionResponse::Link(locations) if locations.len() == 1 => {
                Ok(locations[0].clone())
            }
            _ => Err(anyhow::anyhow!("Got invalid message type: {:?}", response)),
        }
    }

    fn expected_location_link(
        uri: Url,
        source_line: u32,
        source_start_col: u32,
        source_end_col: u32,
        dest_line: u32,
        dest_start_col: u32,
        dest_end_col: u32,
    ) -> LocationLink {
        let source_range = Range::new(
            Position::new(source_line, source_start_col),
            Position::new(source_line, source_end_col),
        );
        let dest_range = Range::new(
            Position::new(dest_line, dest_start_col),
            Position::new(dest_line, dest_end_col),
        );
        LocationLink {
            origin_selection_range: Some(source_range),
            target_uri: uri,
            target_range: dest_range,
            target_selection_range: dest_range,
        }
    }

    fn expected_location_link_from_spans(
        uri: Url,
        source_span: ResolvedSpan,
        dest_span: ResolvedSpan,
    ) -> LocationLink {
        LocationLink {
            origin_selection_range: Some(source_span.into()),
            target_uri: uri,
            target_range: dest_span.into(),
            target_selection_range: dest_span.into(),
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
        let expected_location = expected_location_link(uri.clone(), 3, 6, 13, 1, 4, 11);

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
        let expected_location = expected_location_link(uri.clone(), 3, 6, 13, 1, 4, 11);

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
            <baz_click><baz>b</baz>az</baz_click>()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = expected_location_link_from_spans(
            bar_uri.clone(),
            foo.span("baz_click"),
            bar.span("baz"),
        );

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
            <baz_click><baz>b</baz>az</baz_click>()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = expected_location_link_from_spans(
            bar_uri.clone(),
            foo.span("baz_click"),
            bar.span("baz"),
        );

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
            <baz_click><baz>b</baz>az</baz_click>()
            "#,
        )
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = expected_location_link_from_spans(
            bar_uri.clone(),
            foo.span("baz_click"),
            bar.span("baz"),
        );

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
            <baz_click><baz>b</baz>az</baz_click>()
            "#,
        )
        .replace("{load}", foo_uri.path())
        .trim()
        .to_owned();
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let expected_location = expected_location_link_from_spans(
            foo_uri.clone(),
            foo.span("baz_click"),
            foo.span("baz_loc"),
        );

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

        let expected_location = expected_location_link_from_spans(
            foo_uri.clone(),
            foo.span("not_baz"),
            foo.span("not_baz_loc"),
        );

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
            load("{load}", <baz_click>"<baz>b</baz>az"</baz_click>)
            baz()
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();
        let bar_contents = "def <baz>baz</baz>():\n    pass";
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;

        let expected_location = expected_location_link_from_spans(
            bar_uri.clone(),
            foo.span("baz_click"),
            bar.span("baz"),
        );

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

        let expected_location = expected_location_link_from_spans(
            foo_uri.clone(),
            foo.span("not_baz_loc"),
            foo.span("not_baz_loc"),
        );

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
            load(<bar_click>"{load}"</bar_click>, "baz")
            baz()
            "#,
        )
        .replace("{load}", &bar_load_string)
        .trim()
        .to_owned();
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;

        let expected_location = LocationLink {
            origin_selection_range: Some(foo.span("bar_click").into()),
            target_uri: bar_uri,
            target_range: Default::default(),
            target_selection_range: Default::default(),
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

    #[test]
    fn jumps_to_definitions_in_strings() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let bar_contents = r#""Just <bar>a string</bar>""#;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), bar_contents)?;
        let bar_range = bar.span("bar");
        let bar_range_str = format!(
            "{}:{}:{}:{}",
            bar_range.begin_line, bar_range.begin_column, bar_range.end_line, bar_range.end_column
        );

        let foo_contents = dedent(
            r#"
            <foo1_click>"ba<foo1>r</foo1>.star"</foo1_click>
            [
                <foo2_click>"ba<foo2>r</foo2>.star"</foo2_click>,
                "ignored"
            ]
            {
                <foo3_click>"ba<foo3>r</foo3>.star"</foo3_click>: "ignored",
                "ignored": <foo4_click>"ba<foo4>r</foo4>.star"</foo4_click>,
                "ignored_other": "ignored",
            }

            def f1(x = <foo5_click>"ba<foo5>r</foo5>.star"</foo5_click>):
                <foo6_click>"ba<foo6>r</foo6>.star"</foo6_click>
                [
                    <foo7_click>"ba<foo7>r</foo7>.star"</foo7_click>,
                    "ignored"
                ]
                {
                    <foo8_click>"ba<foo8>r</foo8>.star"</foo8_click>: "ignored",
                    "ignored": <foo9_click>"ba<foo9>r</foo9>.star"</foo9_click>,
                    "ignored_other": "ignored",
                }
                if x == <foo10_click>"ba<foo10>r</foo10>.star"</foo10_click>:
                    <foo11_click>"ba<foo11>r</foo11>.star"</foo11_click>
                    [
                        <foo12_click>"ba<foo12>r</foo12>.star"</foo12_click>,
                        "ignored"
                    ]
                    {
                        <foo13_click>"ba<foo13>r</foo13>.star"</foo13_click>: "ignored",
                        "ignored": <foo14_click>"ba<foo14>r</foo14>.star"</foo14_click>,
                        "ignored_other": "ignored",
                    }
                return <foo15_click>"ba<foo15>r</foo15>.star"</foo15_click>

            foo16 = <foo16_click>"ba<foo16>r</foo16>.star"</foo16_click>

            <baz1_click>"ba<baz1>r</baz1>.star--{bar_range}"</baz1_click>
            [
                <baz2_click>"ba<baz2>r</baz2>.star--{bar_range}"</baz2_click>,
                "ignored"
            ]
            {
                <baz3_click>"ba<baz3>r</baz3>.star--{bar_range}"</baz3_click>: "ignored",
                "ignored": <baz4_click>"ba<baz4>r</baz4>.star--{bar_range}"</baz4_click>,
                "ignored_other": "ignored",
            }

            def f2(x = <baz5_click>"ba<baz5>r</baz5>.star--{bar_range}"</baz5_click>):
                <baz6_click>"ba<baz6>r</baz6>.star--{bar_range}"</baz6_click>
                [
                    <baz7_click>"ba<baz7>r</baz7>.star--{bar_range}"</baz7_click>,
                    "ignored"
                ]
                {
                    <baz8_click>"ba<baz8>r</baz8>.star--{bar_range}"</baz8_click>: "ignored",
                    "ignored": <baz9_click>"ba<baz9>r</baz9>.star--{bar_range}"</baz9_click>,
                    "ignored_other": "ignored",
                }
                if x == <baz10_click>"ba<baz10>r</baz10>.star--{bar_range}"</baz10_click>:
                    <baz11_click>"ba<baz11>r</baz11>.star--{bar_range}"</baz11_click>
                    [
                        <baz12_click>"ba<baz12>r</baz12>.star--{bar_range}"</baz12_click>,
                        "ignored"
                    ]
                    {
                        <baz13_click>"ba<baz13>r</baz13>.star--{bar_range}"</baz13_click>: "ignored",
                        "ignored": <baz14_click>"ba<baz14>r</baz14>.star--{bar_range}"</baz14_click>,
                        "ignored_other": "ignored",
                    }
                return <baz15_click>"ba<baz15>r</baz15>.star--{bar_range}"</baz15_click>

            baz16 = <baz16_click>"ba<baz16>r</baz16>.star--{bar_range}"</baz16_click>
            "#,
        )
        .trim()
        .replace("{bar_range}", &bar_range_str);
        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.open_file(bar_uri.clone(), bar.program())?;

        let mut test = |name: &str, expect_range: bool| -> anyhow::Result<()> {
            let range = if expect_range {
                bar_range
            } else {
                Default::default()
            };
            let expected_location = expected_location_link_from_spans(
                bar_uri.clone(),
                foo.span(&format!("{}_click", name)),
                range,
            );

            let goto_definition = goto_definition_request(
                &mut server,
                foo_uri.clone(),
                foo.begin_line(name),
                foo.begin_column(name),
            );
            let request_id = server.send_request(goto_definition)?;
            let location = goto_definition_response_location(&mut server, request_id)?;

            assert_eq!(expected_location, location);
            Ok(())
        };

        test("foo1", false)?;
        test("foo2", false)?;
        test("foo3", false)?;
        test("foo4", false)?;
        test("foo5", false)?;
        test("foo6", false)?;
        test("foo7", false)?;
        test("foo8", false)?;
        test("foo9", false)?;
        test("foo10", false)?;
        test("foo11", false)?;
        test("foo12", false)?;
        test("foo13", false)?;
        test("foo14", false)?;
        test("foo15", false)?;
        test("foo16", false)?;

        test("baz1", true)?;
        test("baz2", true)?;
        test("baz3", true)?;
        test("baz4", true)?;
        test("baz5", true)?;
        test("baz6", true)?;
        test("baz7", true)?;
        test("baz8", true)?;
        test("baz9", true)?;
        test("baz10", true)?;
        test("baz11", true)?;
        test("baz12", true)?;
        test("baz13", true)?;
        test("baz14", true)?;
        test("baz15", true)?;
        test("baz16", true)?;

        Ok(())
    }

    #[test]
    fn handles_paths_that_are_not_starlark() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");
        let baz_uri = temp_file_uri("baz");
        let dir1_uri = temp_file_uri("dir1");
        let dir2_uri = temp_file_uri("dir2.star");

        let foo_contents = dedent(
            r#"
            <bar>"b<bar_click>a</bar_click>r.star"</bar>
            <baz>"b<baz_click>a</baz_click>z"</baz>
            <dir1>"d<dir1_click>i</dir1_click>r1"</dir1>
            <dir2>"d<dir2_click>i</dir2_click>r2.star"</dir2>
            "#,
        )
        .trim()
        .to_owned();

        let bar_contents = dedent(
            r#"
            def ba r():
                # This has broken syntax
                pass
            "#,
        )
        .trim()
        .to_owned();

        let foo = FixtureWithRanges::from_fixture("foo.star", &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture("bar.star", &bar_contents)?;

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.set_file_contents(PathBuf::from(bar_uri.path()), bar.program())?;
        server.mkdir(dir1_uri.clone());
        server.mkdir(dir2_uri.clone());

        // File with broken syntax
        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("bar_click"),
            foo.begin_column("bar_click"),
        );
        let req_id = server.send_request(goto_definition)?;
        let response = goto_definition_response_location(&mut server, req_id)?;

        let expected = LocationLink {
            origin_selection_range: Some(foo.span("bar").into()),
            target_uri: bar_uri,
            target_range: Default::default(),
            target_selection_range: Default::default(),
        };
        assert_eq!(expected, response);

        // File that is not starlark at all
        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("baz_click"),
            foo.begin_column("baz_click"),
        );
        let req_id = server.send_request(goto_definition)?;
        let response = goto_definition_response_location(&mut server, req_id)?;

        let expected = LocationLink {
            origin_selection_range: Some(foo.span("baz").into()),
            target_uri: baz_uri,
            target_range: Default::default(),
            target_selection_range: Default::default(),
        };
        assert_eq!(expected, response);

        // Directory that doesn't look like a starlark file.
        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("dir1_click"),
            foo.begin_column("dir1_click"),
        );
        let req_id = server.send_request(goto_definition)?;
        let response = goto_definition_response_location(&mut server, req_id)?;

        let expected = LocationLink {
            origin_selection_range: Some(foo.span("dir1").into()),
            target_uri: dir1_uri,
            target_range: Default::default(),
            target_selection_range: Default::default(),
        };
        assert_eq!(expected, response);

        // Directory that looks like a starlark file by name, but isn't
        // File that is not starlark at all
        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("dir2_click"),
            foo.begin_column("dir2_click"),
        );
        let req_id = server.send_request(goto_definition)?;
        let response = goto_definition_response_location(&mut server, req_id)?;

        let expected = LocationLink {
            origin_selection_range: Some(foo.span("dir2").into()),
            target_uri: dir2_uri,
            target_range: Default::default(),
            target_selection_range: Default::default(),
        };
        assert_eq!(expected, response);
        Ok(())
    }
}
