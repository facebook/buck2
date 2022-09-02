/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::future::Future;
use std::io;
use std::io::ErrorKind;
use std::path::Path;

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_common::result::SharedResult;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::Package;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::ProvidersPattern;
use buck2_core::target::TargetName;
use buck2_events::dispatch::instant_event;
use buck2_events::dispatch::span_async;
use buck2_events::dispatch::with_dispatcher;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_interpreter::common::StarlarkPath;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::dice::HasEvents;
use buck2_interpreter::dice::HasGlobalInterpreterState;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use cli_proto::*;
use dice::DiceTransaction;
use futures::channel::mpsc::UnboundedSender;
use futures::FutureExt;
use futures::SinkExt;
use futures::StreamExt;
use gazebo::dupe::Dupe;
use gazebo::prelude::StrExt;
use itertools::Itertools;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_types::Range;
use lsp_types::Url;
use starlark::errors::EvalMessage;
use starlark::lsp::server::server_with_connection;
use starlark::lsp::server::LoadContentsError;
use starlark::lsp::server::LspContext;
use starlark::lsp::server::LspEvalResult;
use starlark::lsp::server::LspUrl;
use starlark::lsp::server::StringLiteralResult;
use starlark::syntax::AstModule;
use starlark::values::docs::Doc;
use starlark::values::docs::Location;
use tonic::Status;

use crate::docs::get_builtin_docs;
use crate::docs::get_prelude_docs;
use crate::streaming_request_handler::StreamingRequestHandler;

static DOCS_DIRECTORY_KEY: &str = "directory";
static DOCS_BUILTIN_KEY: &str = "builtin";

/// Errors when [`LspContext::resolve_load()`] cannot resolve a given path.
#[derive(thiserror::Error, Debug)]
enum ResolveLoadError {
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

/// Store rendered starlark representations of Doc objects for builtin symbols,
/// their names, and their real or virtual paths
struct DocsCache {
    /// Mapping of global names to URLs. These can either be files (for global symbols in the
    /// prelude), or `starlark:` urls for rust native types and functions.
    global_urls: HashMap<String, LspUrl>,
    /// Mapping of starlark: urls to a synthesized starlark representation.
    native_starlark_files: HashMap<LspUrl, String>,
}

#[derive(thiserror::Error, Debug)]
enum DocsCacheError {
    #[error("Duplicate global symbol `{}` detected. Existing URL was `{}`, new URL was `{}`", .name, .existing, .new)]
    DuplicateGlobalSymbol {
        name: String,
        existing: LspUrl,
        new: LspUrl,
    },
}

impl DocsCache {
    async fn get_prelude_uri(
        location: &Location,
        dice_ctx: &DiceTransaction,
        fs: &ProjectRoot,
        cell_resolver: &CellResolver,
    ) -> anyhow::Result<LspUrl> {
        // We strip the "@" off of the path normally to avoid having it in the docs,
        // in paths, etc, but it needs to be put back in to create an import path correctly.
        let path = if location.path.contains("//")
            && !location.path.starts_with('@')
            && !location.path.starts_with("//")
        {
            format!("@{}", location.path)
        } else {
            location.path.to_owned()
        };

        let bfc = BuildFileCell::new(cell_resolver.root_cell().clone());
        let calculator = dice_ctx
            .get_interpreter_calculator(cell_resolver.root_cell(), &bfc)
            .await?;

        let root_import_path = ImportPath::new(
            cell_resolver.get_cell_path(ProjectRelativePath::new("non_existent.bzl")?)?,
            bfc,
        )?;
        let starlark_file = StarlarkPath::LoadFile(&root_import_path);
        let loaded_import_path = calculator.resolve_load(starlark_file, &path).await?;

        let relative_path = cell_resolver.resolve_path(loaded_import_path.borrow().path())?;
        let abs_path = fs.resolve(&relative_path);
        Ok(Url::from_file_path(abs_path).unwrap().try_into()?)
    }

    async fn new(
        builtin_symbols: &[Doc],
        dice_ctx: &DiceTransaction,
        fs: &ProjectRoot,
        cell_resolver: &CellResolver,
    ) -> anyhow::Result<Self> {
        Self::new_with_lookup(builtin_symbols, |location| async {
            Self::get_prelude_uri(location, dice_ctx, fs, cell_resolver).await
        })
        .await
    }

    async fn new_with_lookup<
        'a,
        F: Fn(&'a Location) -> Fut + 'a,
        Fut: Future<Output = anyhow::Result<LspUrl>>,
    >(
        builtin_symbols: &'a [Doc],
        location_lookup: F,
    ) -> anyhow::Result<Self> {
        let mut global_urls = HashMap::with_capacity(builtin_symbols.len());
        let mut native_starlark_files = HashMap::new();
        for doc in builtin_symbols {
            let url = match &doc.id.location {
                Some(l) => location_lookup(l).await?,
                None => {
                    let filename = Path::new(&doc.id.name);
                    let filename = filename.with_extension(match filename.extension() {
                        None => "bzl".to_owned(),
                        Some(e) => format!("{}.bzl", e.to_str().expect("path is UTF-8")),
                    });
                    let path = Path::new("/native")
                        .join(output_subdir_for_doc(doc)?.as_path())
                        .join(filename);

                    let url =
                        LspUrl::try_from(Url::parse(&format!("starlark:{}", path.display()))?)?;

                    native_starlark_files.insert(url.clone(), doc.render_as_code());
                    url
                }
            };
            if let Some(existing) = global_urls.insert(doc.id.name.clone(), url.clone()) {
                return Err(DocsCacheError::DuplicateGlobalSymbol {
                    name: doc.id.name.clone(),
                    existing,
                    new: url,
                }
                .into());
            }
        }
        Ok(Self {
            global_urls,
            native_starlark_files,
        })
    }

    fn native_starlark_file(&self, url: &LspUrl) -> Option<&String> {
        // We only load starlark: urls into this hash map, so other types
        // will just get a "None" back.
        self.native_starlark_files.get(url)
    }

    fn url_for_symbol(&self, symbol: &str) -> Option<&LspUrl> {
        self.global_urls.get(symbol)
    }
}

#[derive(Debug, thiserror::Error)]
enum DocPathError {
    #[error("Directory traversal was found in documentation path `{}` provided for `{}`", .path, .name)]
    InvalidDirectory {
        name: String,
        path: String,
        source: anyhow::Error,
    },
    #[error("Invalid custom attributes were found on `{}`: {}", .name, format_custom_attr_error(.keys_and_values))]
    InvalidCustomAttributes {
        name: String,
        keys_and_values: Vec<(String, String)>,
    },
    #[error("Conflicting custom attributes were found on `{}`: {}", .name, format_custom_attr_error(.keys_and_values))]
    ConflictingCustomAttributes {
        name: String,
        keys_and_values: Vec<(String, String)>,
    },
}

fn format_custom_attr_error(keys_and_values: &[(String, String)]) -> String {
    let mut ret = "{".to_owned();
    ret.push_str(
        &keys_and_values
            .iter()
            .map(|(k, v)| format!("`{}` => `{}`", k, v))
            .join(", "),
    );
    ret.push('}');
    ret
}

/// Get the output subdirectory for a [`Doc`] based on the `directory` custom attr, if present.
pub fn output_subdir_for_doc(doc: &Doc) -> anyhow::Result<ForwardRelativePathBuf> {
    let unknown_keys: Vec<_> = doc
        .custom_attrs
        .iter()
        .filter(|(k, _)| *k != DOCS_DIRECTORY_KEY && *k != DOCS_BUILTIN_KEY)
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    if !unknown_keys.is_empty() {
        return Err(DocPathError::InvalidCustomAttributes {
            name: doc.id.name.to_owned(),
            keys_and_values: unknown_keys,
        }
        .into());
    }

    match (
        doc.custom_attrs.get(DOCS_DIRECTORY_KEY),
        doc.custom_attrs.get(DOCS_BUILTIN_KEY),
    ) {
        (Some(path), None) | (None, Some(path)) => {
            match ForwardRelativePathBuf::new(path.to_owned()) {
                Ok(fp) => Ok(fp),
                Err(e) => Err(DocPathError::InvalidDirectory {
                    name: doc.id.name.to_owned(),
                    path: path.to_owned(),
                    source: e,
                }
                .into()),
            }
        }
        (Some(dir), Some(builtin)) => Err(DocPathError::ConflictingCustomAttributes {
            name: doc.id.name.to_owned(),
            keys_and_values: vec![
                (DOCS_DIRECTORY_KEY.to_owned(), dir.clone()),
                (DOCS_BUILTIN_KEY.to_owned(), builtin.clone()),
            ],
        }
        .into()),
        (None, None) => Ok(ForwardRelativePathBuf::new(String::new())?),
    }
}

struct BuckLspContext {
    dice_ctx: DiceTransaction,
    fs: ProjectRoot,
    cell_resolver: CellResolver,
    docs_cache: DocsCache,
}

impl BuckLspContext {
    fn import_path(&self, path: &Path) -> anyhow::Result<ImportPath> {
        let abs_path = AbsPath::new(path)?;
        let relative_path = self.fs.relativize(abs_path)?;
        let cell_path = self.cell_resolver.get_cell_path(&relative_path)?;

        // Instantiating a BuildFileCell off of the path of the file being originally evaluated.
        // Unlike the build commands and such, there is no meaningful "build file cell" versus
        // the cell of the file being currently evaluated.
        let bfc = BuildFileCell::new(cell_path.cell().clone());

        ImportPath::new(cell_path, bfc)
    }

    fn starlark_import_path(&self, path: &Path) -> anyhow::Result<ImportPath> {
        // The "absolute" path from LSP urls doesn't work here, they have to be relative
        // to get a ProjectRelativePath. We already guaranteed that things start with a '/'
        // (rooted from `starlark:`, see LspUrl), so just drop it.
        let cell_path = self.cell_resolver.get_cell_path(&ProjectRelativePath::new(
            path.to_string_lossy().trim_start_match('/'),
        )?)?;

        let bfc = BuildFileCell::new(cell_path.cell().clone());

        ImportPath::new(cell_path, bfc)
    }

    fn runtime(&self) -> tokio::runtime::Runtime {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
    }

    async fn parse_file_with_contents(
        &self,
        uri: &LspUrl,
        content: String,
    ) -> SharedResult<LspEvalResult> {
        let import_path: ImportPath = match uri {
            LspUrl::File(path) => self.import_path(path),
            LspUrl::Starlark(path) => self.starlark_import_path(path),
            LspUrl::Other(_) => Err(LoadContentsError::WrongScheme(
                "file:// or starlark:".to_owned(),
                uri.clone(),
            )
            .into()),
        }?;

        let calculator = self
            .dice_ctx
            .get_interpreter_calculator(import_path.cell(), import_path.build_file_cell())
            .await?;

        let path = StarlarkPath::LoadFile(&import_path);
        let ast = calculator.prepare_eval_with_content(path, content).await?;
        Ok(LspEvalResult {
            diagnostics: vec![],
            ast: Some(ast),
        })
    }

    async fn parse_file_from_string(
        &self,
        current_package: &Package,
        literal: &str,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        match ForwardRelativePath::new(literal) {
            Ok(package_relative) => {
                let relative_path = self
                    .cell_resolver
                    .resolve_path(current_package.join(package_relative).as_cell_path())?;

                let path = self.fs.resolve(&relative_path);
                let url = Url::from_file_path(path).unwrap().try_into()?;
                let string_literal = StringLiteralResult {
                    url,
                    location_finder: None,
                };
                Ok(Some(string_literal))
            }
            Err(_) => Ok(None),
        }
    }

    async fn parse_target_from_string(
        &self,
        current_package: &Package,
        literal: &str,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        let cell = self.cell_resolver.get(current_package.cell_name())?;
        match ParsedPattern::<ProvidersPattern>::parsed_opt_absolute(
            cell.cell_alias_resolver(),
            Some(current_package),
            literal,
        ) {
            Ok(ParsedPattern::Target(package, (target, _))) => {
                let res = self
                    .dice_ctx
                    .get_package_listing_resolver()
                    .resolve(&package)
                    .await
                    .and_then(|listing| {
                        let relative_path = self
                            .cell_resolver
                            .resolve_package(&package)?
                            .join(listing.buildfile());
                        let path = self.fs.resolve(&relative_path);
                        match Url::from_file_path(path).unwrap().try_into() {
                            Ok(url) => {
                                let string_literal = StringLiteralResult {
                                    url,
                                    location_finder: Some(box |ast, _url| {
                                        Ok(Self::find_target(ast, target))
                                    }),
                                };
                                Ok(Some(string_literal))
                            }
                            Err(e) => Err(anyhow::Error::from(e).into()),
                        }
                    })?;
                Ok(res)
            }
            _ => Ok(None),
        }
    }

    fn find_target(ast: &AstModule, target: TargetName) -> Option<Range> {
        ast.find_function_call_with_name(target.value())
            .map(Range::from)
    }
}

impl LspContext for BuckLspContext {
    fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult {
        let dispatcher = self.dice_ctx.per_transaction_data().get_dispatcher().dupe();
        self.runtime()
            .block_on(with_dispatcher_async(dispatcher, async {
                match uri {
                    LspUrl::File(_) | LspUrl::Starlark(_) => {
                        match self.parse_file_with_contents(uri, content).await {
                            Ok(result) => result,
                            Err(e) => {
                                let message = EvalMessage::from_anyhow(uri.path(), e.inner());
                                LspEvalResult {
                                    diagnostics: vec![message.into()],
                                    ast: None,
                                }
                            }
                        }
                    }
                    _ => LspEvalResult::default(),
                }
            }))
    }

    fn resolve_load(&self, path: &str, current_file: &LspUrl) -> anyhow::Result<LspUrl> {
        let dispatcher = self.dice_ctx.per_transaction_data().get_dispatcher().dupe();
        self.runtime()
            .block_on(with_dispatcher_async(dispatcher, async {
                match current_file {
                    LspUrl::File(current_file) => {
                        let current_import_path = self.import_path(current_file)?;
                        let calculator = self
                            .dice_ctx
                            .get_interpreter_calculator(
                                current_import_path.cell(),
                                current_import_path.build_file_cell(),
                            )
                            .await?;

                        let starlark_file = StarlarkPath::LoadFile(&current_import_path);
                        let loaded_import_path =
                            calculator.resolve_load(starlark_file, path).await?;
                        let relative_path = self
                            .cell_resolver
                            .resolve_path(loaded_import_path.borrow().path())?;
                        let abs_path = self.fs.resolve(&relative_path);
                        let url = Url::from_file_path(abs_path).unwrap().try_into()?;

                        Ok(url)
                    }
                    _ => Err(ResolveLoadError::WrongScheme(
                        "file://".to_owned(),
                        current_file.clone(),
                    )
                    .into()),
                }
            }))
    }

    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &LspUrl,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        let import_path = match current_file {
            LspUrl::File(current_file) => Ok(self.import_path(current_file.parent().unwrap())?),
            _ => Err(ResolveLoadError::WrongScheme(
                "file://".to_owned(),
                current_file.clone(),
            )),
        }?;
        let current_package = Package::from_cell_path(import_path.path());
        let dispatcher = self.dice_ctx.per_transaction_data().get_dispatcher().dupe();
        self.runtime()
            .block_on(with_dispatcher_async(dispatcher, async {
                // Right now we swallow the errors up as they can happen for a lot of reasons that are
                // perfectly recoverable (e.g. an invalid cell is specified, we can't list an invalid
                // package path, an absolute path is requested, etc).
                //
                // anyhow::Error sort of obscures the root causes, so we just have
                // to assume if it failed, it's fine, and we can maybe try the next thing.
                if let Ok(Some(string_literal)) = self
                    .parse_target_from_string(&current_package, literal)
                    .await
                {
                    Ok(Some(string_literal))
                } else if let Ok(Some(string_literal)) =
                    self.parse_file_from_string(&current_package, literal).await
                {
                    Ok(Some(string_literal))
                } else {
                    Ok(None)
                }
            }))
    }

    fn get_load_contents(&self, uri: &LspUrl) -> anyhow::Result<Option<String>> {
        let dispatcher = self.dice_ctx.per_transaction_data().get_dispatcher().dupe();
        self.runtime()
            .block_on(with_dispatcher_async(dispatcher, async {
                match uri {
                    LspUrl::File(path) => {
                        let path = self.import_path(path)?;
                        match self.dice_ctx.file_ops().read_file(path.path()).await {
                            Ok(s) => Ok(Some(s)),
                            Err(e) => match e.downcast_ref::<io::Error>() {
                                Some(inner_e) if inner_e.kind() == ErrorKind::NotFound => Ok(None),
                                _ => Err(e),
                            },
                        }
                    }
                    LspUrl::Starlark(_) => Ok(self.docs_cache.native_starlark_file(uri).cloned()),
                    _ => Err(
                        LoadContentsError::WrongScheme("file://".to_owned(), uri.clone()).into(),
                    ),
                }
            }))
    }

    fn get_url_for_global_symbol(
        &self,
        _current_file: &LspUrl,
        symbol: &str,
    ) -> anyhow::Result<Option<LspUrl>> {
        Ok(self.docs_cache.url_for_symbol(symbol).cloned())
    }
}

pub(crate) async fn run_lsp_server_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    req: StreamingRequestHandler<cli_proto::LspRequest>,
) -> anyhow::Result<cli_proto::LspResponse> {
    let metadata = ctx.request_metadata()?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::LspCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result = run_lsp_server(ctx, req).await;
        let end_event = command_end(metadata, &result, buck2_data::LspCommandEnd {});
        (result, end_event)
    })
    .await
}

/// Run an LSP server for a given client.
async fn run_lsp_server(
    ctx: Box<dyn ServerCommandContextTrait>,
    mut req: StreamingRequestHandler<LspRequest>,
) -> anyhow::Result<LspResponse> {
    ctx.with_dice_ctx(async move |ctx, dice_ctx| {
        let cell_resolver: CellResolver = dice_ctx.get_cell_resolver().await?;
        let fs = ctx.project_root().clone();

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

        let interpreter_state = dice_ctx.get_global_interpreter_state().await?.dupe();
        let mut builtin_docs = get_builtin_docs(
            cell_resolver.root_cell_cell_alias_resolver().dupe(),
            interpreter_state,
        )?;
        let builtin_names = builtin_docs.iter().map(|d| d.id.name.as_str()).collect();
        let prelude_docs = get_prelude_docs(&dice_ctx, &builtin_names).await?;
        builtin_docs.extend(prelude_docs);

        let docs_cache = DocsCache::new(&builtin_docs, &dice_ctx, &fs, &cell_resolver).await?;

        let recv_thread = std::thread::spawn(move || {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .build()
                .unwrap();
            runtime.block_on(recv_from_lsp(client_receiver, events_from_server))
        });

        let dispatcher = dice_ctx.per_transaction_data().get_dispatcher().dupe();
        let server_thread = std::thread::spawn(with_dispatcher(dispatcher, || {
            move || {
                server_with_connection(
                    connection,
                    BuckLspContext {
                        dice_ctx,
                        fs,
                        cell_resolver,
                        docs_cache,
                    },
                )
            }
        }));

        let res = loop {
            let message_handler_res = tokio::select! {
                m = req.message().fuse() => {
                    handle_incoming_lsp_message(&send_to_server, m)
                },
                m = events_to_client.next() => {
                    Ok(handle_outgoing_lsp_message(m))
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
    })
    .await
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
fn handle_outgoing_lsp_message(event: Option<buck2_data::LspResult>) -> Option<LspResponse> {
    match event {
        Some(event) => {
            instant_event(event);
            None
        }
        None => Some(LspResponse {}),
    }
}

#[cfg(test)]
mod test {
    use lsp_types::Url;
    use maplit::hashmap;
    use starlark::lsp::server::LspUrl;
    use starlark::values::docs::Doc;
    use starlark::values::docs::DocItem;
    use starlark::values::docs::Function;
    use starlark::values::docs::Identifier;
    use starlark::values::docs::Location;

    use crate::lsp::DocsCache;
    use crate::lsp::DOCS_DIRECTORY_KEY;

    #[test]
    fn cache_builds() -> anyhow::Result<()> {
        let docs = vec![
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
                custom_attrs: hashmap! { DOCS_DIRECTORY_KEY.to_owned() => "subdir".to_owned() },
            },
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
        ];

        let runtime = tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap();
        async fn lookup_function(location: &Location) -> anyhow::Result<LspUrl> {
            match location.path.as_str() {
                "//dir/prelude.bzl" => Ok(LspUrl::try_from(Url::parse(
                    "file:///usr/local/dir/prelude.bzl",
                )?)?),
                p => Err(anyhow::anyhow!("Unknown path {}", p)),
            }
        }
        let cache =
            runtime.block_on(async { DocsCache::new_with_lookup(&docs, lookup_function).await })?;

        assert_eq!(
            &LspUrl::try_from(Url::parse("starlark:/native/native_function1.bzl")?)?,
            cache.url_for_symbol("native_function1").unwrap()
        );
        assert_eq!(
            &LspUrl::try_from(Url::parse("starlark:/native/subdir/native_function2.bzl")?)?,
            cache.url_for_symbol("native_function2").unwrap()
        );
        assert_eq!(
            &LspUrl::try_from(Url::parse("file:/usr/local/dir/prelude.bzl")?)?,
            cache.url_for_symbol("prelude_function").unwrap()
        );

        Ok(())
    }
}
