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
use derive_more::Display;
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
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;

use crate::analysis::Definition;
use crate::analysis::DottedDefinition;
use crate::analysis::IdentifierDefinition;
use crate::analysis::LspModule;
use crate::codemap::ResolvedSpan;
use crate::lsp::server::LoadContentsError::WrongScheme;
use crate::syntax::AstModule;

/// The request to get the file contents for a starlark: URI
struct StarlarkFileContentsRequest {}

impl lsp_types::request::Request for StarlarkFileContentsRequest {
    type Params = StarlarkFileContentsParams;
    type Result = StarlarkFileContentsResponse;
    const METHOD: &'static str = "starlark/fileContents";
}

/// Params to get the file contents for a starlark: URI.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")] // camelCase to match idioms in LSP spec / typescript land.
struct StarlarkFileContentsParams {
    uri: LspUrl,
}

/// The contents of a starlark: URI if available.
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
#[serde(rename_all = "camelCase")] // camelCase to match idioms in LSP spec / typescript land.
struct StarlarkFileContentsResponse {
    contents: Option<String>,
}

/// Errors that can happen when converting LspUrl and Url to/from each other.
#[derive(thiserror::Error, Debug)]
pub enum LspUrlError {
    /// The path component of the URL was not absolute. This is required for all supported
    /// URL types.
    #[error("`{}` does not have an absolute path component", .0)]
    NotAbsolute(Url),
    /// For some reason the PathBuf/Url in the LspUrl could not be converted back to a URL.
    #[error("`{}` could not be converted back to a URL", .0)]
    Unparseable(LspUrl),
}

/// A URL that represents the two types (plus an "Other") of URIs that are supported.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Display)]
pub enum LspUrl {
    /// A "file://" url with a path sent from the LSP client.
    #[display(fmt = "file://{}", "_0.display()")]
    File(PathBuf),
    /// A "starlark:" url. This is mostly used for native types that don't actually
    /// exist on the filesystem. The path component always has a leading slash.
    #[display(fmt = "starlark:{}", "_0.display()")]
    Starlark(PathBuf),
    /// Any other type. Often should just be ignored, or return an error.
    #[display(fmt = "{}", "_0")]
    Other(Url),
}

impl Serialize for LspUrl {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match Url::try_from(self) {
            Ok(url) => url.serialize(serializer),
            Err(e) => Err(serde::ser::Error::custom(e.to_string())),
        }
    }
}

impl<'de> Deserialize<'de> for LspUrl {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let url = Url::deserialize(deserializer)?;
        LspUrl::try_from(url).map_err(|e| serde::de::Error::custom(e.to_string()))
    }
}

impl LspUrl {
    /// Returns the path component of the underlying URL
    pub fn path(&self) -> &Path {
        match self {
            LspUrl::File(p) => p.as_path(),
            LspUrl::Starlark(p) => p.as_path(),
            LspUrl::Other(u) => Path::new(u.path()),
        }
    }
}

impl TryFrom<Url> for LspUrl {
    type Error = LspUrlError;

    fn try_from(url: Url) -> Result<Self, Self::Error> {
        match url.scheme() {
            "file" => {
                let path = PathBuf::from(url.path());
                if path.to_string_lossy().starts_with('/') {
                    Ok(Self::File(path))
                } else {
                    Err(LspUrlError::NotAbsolute(url))
                }
            }
            "starlark" => {
                let path = PathBuf::from(url.path());
                // Use "starts with a /" because, while leading slashes are accepted on
                // windows, they do not report "true" from `is_absolute()`.
                if path.to_string_lossy().starts_with('/') {
                    Ok(Self::Starlark(path))
                } else {
                    Err(LspUrlError::NotAbsolute(url))
                }
            }
            _ => Ok(Self::Other(url)),
        }
    }
}

impl TryFrom<LspUrl> for Url {
    type Error = LspUrlError;

    fn try_from(url: LspUrl) -> Result<Self, Self::Error> {
        Url::try_from(&url)
    }
}

impl TryFrom<&LspUrl> for Url {
    type Error = LspUrlError;

    fn try_from(url: &LspUrl) -> Result<Self, Self::Error> {
        match &url {
            LspUrl::File(p) => {
                Url::from_file_path(&p).map_err(|_| LspUrlError::Unparseable(url.clone()))
            }
            LspUrl::Starlark(p) => Url::parse(&format!("starlark:{}", p.display()))
                .map_err(|_| LspUrlError::Unparseable(url.clone())),
            LspUrl::Other(u) => Ok(u.clone()),
        }
    }
}

/// The result of resolving a StringLiteral when looking up a definition.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct StringLiteralResult {
    /// The path that a string literal resolves to.
    pub url: LspUrl,
    /// A function that takes the AstModule at path specified by `url` and that same url, and
    /// allows resolving a location to jump to within the specific URL if desired.
    ///
    /// If `None`, then just jump to the URL. Do not attempt to load the file.
    #[derivative(Debug = "ignore")]
    pub location_finder:
        Option<Box<dyn FnOnce(&AstModule, &LspUrl) -> anyhow::Result<Option<Range>>>>,
}

/// The result of evaluating a starlark program for use in the LSP.
#[derive(Default)]
pub struct LspEvalResult {
    /// The list of diagnostic issues that were encountered while evaluating a starlark program.
    pub diagnostics: Vec<Diagnostic>,
    /// If the program could be parsed, the parsed module.
    pub ast: Option<AstModule>,
}

/// Settings that the LspContext can provide to change what capabilities the server enables
/// or disables.
#[derive(Dupe, Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct LspServerSettings {
    /// Whether goto definition should work.
    pub enable_goto_definition: bool,
}

impl Default for LspServerSettings {
    fn default() -> Self {
        Self {
            enable_goto_definition: true,
        }
    }
}

/// Various pieces of context to allow the LSP to interact with starlark parsers, etc.
pub trait LspContext {
    /// Parse a file with the given contents. The filename is used in the diagnostics.
    fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult;

    /// Resolve a path given in a `load()` statement.
    ///
    /// `path` is the string representation in the `load()` statement. Its meaning is
    ///        implementation defined.
    /// `current_file` is the the file that is including the `load()` statement, and should be used
    ///                if `path` is "relative" in a semantic sense.
    fn resolve_load(&self, path: &str, current_file: &LspUrl) -> anyhow::Result<LspUrl>;

    /// Resolve a string literal into a Url and a function that specifies a locaction within that
    /// target file.
    ///
    /// This can be used for things like file paths in string literals, build targets, etc.
    ///
    /// `current_file` is the file that is currently being evaluated
    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &LspUrl,
    ) -> anyhow::Result<Option<StringLiteralResult>>;

    /// Get the contents of a starlark program at a given path, if it exists.
    fn get_load_contents(&self, uri: &LspUrl) -> anyhow::Result<Option<String>>;

    /// Get the contents of a file at a given URI, and attempt to parse it.
    fn parse_file(&self, uri: &LspUrl) -> anyhow::Result<Option<LspEvalResult>> {
        let result = self
            .get_load_contents(uri)?
            .map(|content| self.parse_file_with_contents(uri, content));
        Ok(result)
    }

    /// Get the LSPUrl for a global symbol if possible.
    ///
    /// The current file is provided in case different files have different global symbols
    /// defined.
    fn get_url_for_global_symbol(
        &self,
        current_file: &LspUrl,
        symbol: &str,
    ) -> anyhow::Result<Option<LspUrl>>;
}

/// Errors when [`LspContext::resolve_load()`] cannot resolve a given path.
#[derive(thiserror::Error, Debug)]
enum ResolveLoadError {
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

/// Errors when loading contents of a starlark program.
#[derive(thiserror::Error, Debug)]
pub enum LoadContentsError {
    /// The provided Url was not absolute and it needs to be.
    #[error("Path for URL `{}` was not absolute", .0)]
    NotAbsolute(LspUrl),
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

struct Backend<T: LspContext> {
    connection: Connection,
    context: T,
    /// The `AstModule` from the last time that a file was opened / changed and parsed successfully.
    /// Entries are evicted when the file is closed.
    last_valid_parse: RwLock<HashMap<LspUrl, Arc<LspModule>>>,
}

/// The logic implementations of stuff
impl<T: LspContext> Backend<T> {
    fn server_capabilities(settings: LspServerSettings) -> ServerCapabilities {
        let definition_provider = settings.enable_goto_definition.then_some({
            OneOf::Right(DefinitionOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })
        });
        ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            definition_provider,
            ..ServerCapabilities::default()
        }
    }

    fn get_ast(&self, uri: &LspUrl) -> Option<Arc<LspModule>> {
        let last_valid_parse = self.last_valid_parse.read().unwrap();
        last_valid_parse.get(uri).duped()
    }

    fn get_ast_or_load_from_disk(&self, uri: &LspUrl) -> anyhow::Result<Option<Arc<LspModule>>> {
        let module = match self.get_ast(uri) {
            Some(result) => Some(result),
            None => self
                .context
                .parse_file(uri)?
                .and_then(|eval_result| eval_result.ast.map(|ast| Arc::new(LspModule::new(ast)))),
        };
        Ok(module)
    }

    fn validate(&self, uri: Url, version: Option<i64>, text: String) -> anyhow::Result<()> {
        let uri = uri.try_into()?;
        let eval_result = self.context.parse_file_with_contents(&uri, text);
        if let Some(ast) = eval_result.ast {
            let module = Arc::new(LspModule::new(ast));
            let mut last_valid_parse = self.last_valid_parse.write().unwrap();
            last_valid_parse.insert(uri.clone(), module);
        }
        self.publish_diagnostics(uri.try_into()?, eval_result.diagnostics, version);
        Ok(())
    }

    fn did_open(&self, params: DidOpenTextDocumentParams) -> anyhow::Result<()> {
        self.validate(
            params.text_document.uri,
            Some(params.text_document.version as i64),
            params.text_document.text,
        )
    }

    fn did_change(&self, params: DidChangeTextDocumentParams) -> anyhow::Result<()> {
        // We asked for Sync full, so can just grab all the text from params
        let change = params.content_changes.into_iter().next().unwrap();
        self.validate(
            params.text_document.uri,
            Some(params.text_document.version as i64),
            change.text,
        )
    }

    fn did_close(&self, params: DidCloseTextDocumentParams) -> anyhow::Result<()> {
        {
            let mut last_valid_parse = self.last_valid_parse.write().unwrap();
            last_valid_parse.remove(&params.text_document.uri.clone().try_into()?);
        }
        self.publish_diagnostics(params.text_document.uri, Vec::new(), None);
        Ok(())
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

    /// Get the file contents of a starlark: URI.
    fn get_starlark_file_contents(&self, id: RequestId, params: StarlarkFileContentsParams) {
        let response: anyhow::Result<_> = match params.uri {
            LspUrl::Starlark(_) => self
                .context
                .get_load_contents(&params.uri)
                .map(|contents| StarlarkFileContentsResponse { contents }),
            _ => Err(WrongScheme("starlark:".to_owned(), params.uri).into()),
        };
        self.send_response(new_response(id, response));
    }

    fn resolve_load_path(&self, path: &str, current_uri: &LspUrl) -> anyhow::Result<LspUrl> {
        match current_uri {
            LspUrl::File(_) => self.context.resolve_load(path, current_uri),
            LspUrl::Starlark(_) | LspUrl::Other(_) => {
                Err(ResolveLoadError::WrongScheme("file://".to_owned(), current_uri.clone()).into())
            }
        }
    }

    /// Simple helper to generate `Some(LocationLink)` objects in `resolve_definition_location`
    fn location_link<R: Into<Range> + Copy>(
        source: ResolvedSpan,
        uri: &LspUrl,
        target_range: R,
    ) -> anyhow::Result<Option<LocationLink>> {
        Ok(Some(LocationLink {
            origin_selection_range: Some(source.into()),
            target_uri: uri.try_into()?,
            target_range: target_range.into(),
            target_selection_range: target_range.into(),
        }))
    }

    /// Find the ultimate places that an identifier is defined.
    ///
    /// Takes a definition location and if necesary loads other files trying
    /// to find where the symbol was defined in a useful way. e.g. pointing to the
    /// symbol in a "load()" statement isn't useful, but going to the file it is
    /// loaded from and pointing at a function definition very much is.
    fn resolve_definition_location(
        &self,
        definition: IdentifierDefinition,
        source: ResolvedSpan,
        member: Option<&str>,
        uri: LspUrl,
    ) -> anyhow::Result<Option<LocationLink>> {
        let ret = match definition {
            IdentifierDefinition::Location {
                destination: target,
                ..
            } => Self::location_link(source, &uri, target)?,
            IdentifierDefinition::LoadedLocation {
                destination: location,
                path,
                name,
                ..
            } => {
                let load_uri = self.resolve_load_path(&path, &uri)?;
                let loaded_location =
                    self.get_ast_or_load_from_disk(&load_uri)?
                        .and_then(|ast| match member {
                            Some(member) => ast.find_exported_symbol_and_member(&name, member),
                            None => ast.find_exported_symbol(&name),
                        });
                match loaded_location {
                    None => Self::location_link(source, &uri, location)?,
                    Some(loaded_location) => {
                        Self::location_link(source, &load_uri, loaded_location)?
                    }
                }
            }
            IdentifierDefinition::NotFound => None,
            IdentifierDefinition::LoadPath { path, .. } => {
                match self.resolve_load_path(&path, &uri) {
                    Ok(load_uri) => Self::location_link(source, &load_uri, Range::default())?,
                    Err(_) => None,
                }
            }
            IdentifierDefinition::StringLiteral { literal, .. } => {
                let literal = self.context.resolve_string_literal(&literal, &uri)?;
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
                        Self::location_link(source, &url, target_range)?
                    }
                    Some(StringLiteralResult {
                        url,
                        location_finder: None,
                    }) => Self::location_link(source, &url, Range::default())?,
                    _ => None,
                }
            }
            IdentifierDefinition::Unresolved { name, .. } => {
                match self.context.get_url_for_global_symbol(&uri, &name)? {
                    Some(uri) => {
                        let loaded_location =
                            self.get_ast_or_load_from_disk(&uri)?
                                .and_then(|ast| match member {
                                    Some(member) => {
                                        ast.find_exported_symbol_and_member(&name, member)
                                    }
                                    None => ast.find_exported_symbol(&name),
                                });

                        Self::location_link(source, &uri, loaded_location.unwrap_or_default())?
                    }
                    None => None,
                }
            }
        };
        Ok(ret)
    }

    fn find_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> anyhow::Result<GotoDefinitionResponse> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .try_into()?;
        let line = params.text_document_position_params.position.line;
        let character = params.text_document_position_params.position.character;

        let location = match self.get_ast(&uri) {
            Some(ast) => {
                let location = ast.find_definition(line, character);
                let source = location.source().unwrap_or_default();
                match location {
                    Definition::Identifier(definition) => {
                        self.resolve_definition_location(definition, source, None, uri)?
                    }
                    // In this case we don't pass the name along in the root_definition_location,
                    // so it's simpler to do the lookup here, rather than threading a ton of
                    // information through.
                    Definition::Dotted(DottedDefinition {
                        root_definition_location: IdentifierDefinition::Location { .. },
                        segments,
                        ..
                    }) => {
                        let member_location = ast.find_exported_symbol_and_member(
                            segments.first().expect("at least one segment").as_str(),
                            segments.get(1).expect("at least two segments").as_str(),
                        );
                        Self::location_link(source, &uri, member_location.unwrap_or_default())?
                    }
                    Definition::Dotted(definition) => self.resolve_definition_location(
                        definition.root_definition_location,
                        source,
                        Some(
                            definition
                                .segments
                                .last()
                                .expect("to have at least one component")
                                .as_str(),
                        ),
                        uri,
                    )?,
                }
            }
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
        self.log_message(MessageType::INFO, "Starlark server initialised");
        for msg in &self.connection.receiver {
            match msg {
                Message::Request(req) => {
                    // TODO(nmj): Also implement DocumentSymbols so that some logic can
                    //            be handled client side.
                    if let Some(params) = as_request::<GotoDefinition>(&req) {
                        self.goto_definition(req.id, params);
                    } else if let Some(params) = as_request::<StarlarkFileContentsRequest>(&req) {
                        self.get_starlark_file_contents(req.id, params);
                    } else if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    // Currently don't handle any other requests
                }
                Message::Notification(x) => {
                    if let Some(params) = as_notification::<DidOpenTextDocument>(&x) {
                        self.did_open(params)?;
                    } else if let Some(params) = as_notification::<DidChangeTextDocument>(&x) {
                        self.did_change(params)?;
                    } else if let Some(params) = as_notification::<DidCloseTextDocument>(&x) {
                        self.did_close(params)?;
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
    let (init_request_id, init_value) = connection.initialize_start()?;

    let initialization_params: InitializeParams = serde_json::from_value(init_value)?;
    let server_settings = initialization_params
        .initialization_options
        .as_ref()
        .and_then(|opts| serde_json::from_value(opts.clone()).ok())
        .unwrap_or_default();
    let capabilities_payload = Backend::<T>::server_capabilities(server_settings);
    let server_capabilities = serde_json::to_value(&capabilities_payload).unwrap();

    let initialize_data = serde_json::json!({
            "capabilities": server_capabilities,
    });
    connection.initialize_finish(init_request_id, initialize_data)?;

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

    use anyhow::Context;
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
    use crate::lsp::server::LspServerSettings;
    use crate::lsp::server::LspUrl;
    use crate::lsp::server::StarlarkFileContentsParams;
    use crate::lsp::server::StarlarkFileContentsRequest;
    use crate::lsp::server::StarlarkFileContentsResponse;
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

    #[test]
    fn disables_goto_definition() -> anyhow::Result<()> {
        let server = TestServer::new_with_settings(Some(LspServerSettings {
            enable_goto_definition: false,
        }))?;

        let goto_definition_disabled = server
            .initialization_result()
            .unwrap()
            .capabilities
            .definition_provider
            .is_none();

        assert!(goto_definition_disabled);

        let server = TestServer::new_with_settings(Some(LspServerSettings {
            enable_goto_definition: true,
        }))?;

        let goto_definition_enabled = server
            .initialization_result()
            .unwrap()
            .capabilities
            .definition_provider
            .is_some();

        assert!(goto_definition_enabled);
        Ok(())
    }

    #[test]
    fn returns_starlark_file_contents() -> anyhow::Result<()> {
        let mut server = TestServer::new()?;

        let uri = LspUrl::try_from(Url::parse("starlark:/native/builtin.bzl")?)?;
        let req = server.new_request::<StarlarkFileContentsRequest>(StarlarkFileContentsParams {
            uri: uri.clone(),
        });
        let request_id = server.send_request(req)?;
        let response = server.get_response::<StarlarkFileContentsResponse>(request_id)?;
        assert_eq!(
            server.docs_as_code(&uri).unwrap(),
            response.contents.unwrap()
        );

        let req = server.new_request::<StarlarkFileContentsRequest>(StarlarkFileContentsParams {
            uri: LspUrl::try_from(Url::parse("starlark:/native/not_builtin.bzl")?)?,
        });
        let request_id = server.send_request(req)?;
        let response = server.get_response::<StarlarkFileContentsResponse>(request_id)?;
        assert!(response.contents.is_none());

        Ok(())
    }

    #[test]
    fn goto_works_for_native_symbols() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let native_uri = Url::parse("starlark:/native/builtin.bzl")?;

        let mut server = TestServer::new()?;

        let foo_contents = dedent(
            r#"
            <click_n1>na<n1>t</n1>ive_function1</click_n1>()
            def f(<n2_loc>native_function1</n2_loc>):
                print(<click_n2>nat<n2>i</n2>ve_function1</click_n2>)
            mi<n3>s</n3>sing_global()
            "#,
        )
        .trim()
        .to_owned();
        let native_contents = dedent(
            r#"
            def <n1_loc>native_function1</n1_loc>():
                pass

            def native_function2():
                pass
            "#,
        )
        .trim()
        .to_owned();

        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let native = FixtureWithRanges::from_fixture(native_uri.path(), &native_contents)?;

        server.open_file(foo_uri.clone(), foo.program())?;

        let expected_n1_location = expected_location_link_from_spans(
            native_uri,
            foo.span("click_n1"),
            native.span("n1_loc"),
        );

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("n1"),
            foo.begin_column("n1"),
        );
        let request_id = server.send_request(goto_definition)?;
        let n1_location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_n1_location, n1_location);

        let expected_n2_location = expected_location_link_from_spans(
            foo_uri.clone(),
            foo.span("click_n2"),
            foo.span("n2_loc"),
        );

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri.clone(),
            foo.begin_line("n2"),
            foo.begin_column("n2"),
        );
        let request_id = server.send_request(goto_definition)?;
        let n2_location = goto_definition_response_location(&mut server, request_id)?;

        assert_eq!(expected_n2_location, n2_location);

        let goto_definition = goto_definition_request(
            &mut server,
            foo_uri,
            foo.begin_line("n3"),
            foo.begin_column("n3"),
        );
        let request_id = server.send_request(goto_definition)?;
        let n3_response = server.get_response::<GotoDefinitionResponse>(request_id)?;
        match n3_response {
            GotoDefinitionResponse::Array(definitions) if definitions.is_empty() => Ok(()),
            response => Err(anyhow::anyhow!(
                "Expected empty definitions, got `{:?}`",
                response
            )),
        }?;

        Ok(())
    }

    #[test]
    fn jumps_to_original_member_definition() -> anyhow::Result<()> {
        let foo_uri = temp_file_uri("foo.star");
        let bar_uri = temp_file_uri("bar.star");

        let foo_contents = dedent(
            r#"
            load("{load}", "loaded")

            def <dest_baz>_baz</dest_baz>():
                pass

            <dest_quz>_quz</dest_quz> = 6

            <dest_foobar>FooBarModule</dest_foobar> = 5
            FooModule = struct(<dest_foo>foo</dest_foo> = 5)
            # Member value does not exist
            BarModule = struct(<dest_bar>bar</dest_bar> = _bar)
            BazModule = struct(
                bar = _bar,
                baz = _baz,
            )
            QuzModule = struct(bar = _bar, baz = _baz, quz = _quz)

            FooBarModule.<foobar>f<foobar_click>o</foobar_click>obar</foobar>
            FooModule.<foo>f<foo_click>o</foo_click>o</foo>
            BarModule.<bar>b<bar_click>a</bar_click>r</bar>
            BazModule.<baz>b<baz_click>a</baz_click>z</baz>
            QuzModule.<quz>q<quz_click>u</quz_click>z</quz>
            loaded.<x><x_click>x</x_click></x>
            loaded.<y><y_click>y</y_click></y>
            "#,
        )
        .replace("{load}", bar_uri.path())
        .trim()
        .to_owned();

        let bar_contents = dedent(
            r#"
            def <dest_x>_x</dest_x>():
                pass
            <dest_y>loaded</dest_y> = struct(x = _x)
            "#,
        )
        .trim()
        .to_owned();

        let foo = FixtureWithRanges::from_fixture(foo_uri.path(), &foo_contents)?;
        let bar = FixtureWithRanges::from_fixture(bar_uri.path(), &bar_contents)?;

        let mut server = TestServer::new()?;
        server.open_file(foo_uri.clone(), foo.program())?;
        server.open_file(bar_uri.clone(), bar.program())?;

        let cases = [
            (&foo, &foo_uri, "foobar"),
            (&foo, &foo_uri, "foo"),
            (&foo, &foo_uri, "bar"),
            (&foo, &foo_uri, "baz"),
            (&foo, &foo_uri, "quz"),
            (&bar, &bar_uri, "x"),
            (&bar, &bar_uri, "y"),
        ];

        let expected_results = cases
            .iter()
            .map(|(fixture, uri, id)| {
                expected_location_link_from_spans(
                    (*uri).clone(),
                    foo.span(id),
                    fixture.span(&format!("dest_{}", id)),
                )
            })
            .collect::<Vec<_>>();

        let requests = cases
            .iter()
            .map(|(_, _, id)| {
                goto_definition_request(
                    &mut server,
                    foo_uri.clone(),
                    foo.begin_line(&format!("{}_click", id)),
                    foo.begin_column(&format!("{}_click", id)),
                )
            })
            .collect::<Vec<_>>();

        for (case, request, expected) in itertools::izip!(cases, requests, expected_results) {
            let req_id = server.send_request(request)?;
            let response = goto_definition_response_location(&mut server, req_id)
                .context(format!("getting response for case `{}`", case.2))?;

            assert_eq!(
                expected, response,
                "Incorrect response for case `{}`",
                case.2
            );
        }
        Ok(())
    }
}
