/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::future::Future;
use std::path::Path;
use std::thread;

use buck2_build_api::actions::artifact::get_artifact_fs::GetArtifactFs;
use buck2_cli_proto::*;
use buck2_common::buildfiles::HasBuildfiles;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::file_ops::dice::DiceFileComputations;
use buck2_common::package_listing::dice::DicePackageListingResolver;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bxl::BxlFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::CellResolver;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::source_path::SourcePath;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern::TargetParsingRel;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::target::name::TargetName;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use buck2_events::dispatch::span_async;
use buck2_events::dispatch::with_dispatcher;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_fs::paths::abs_path::AbsPath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_interpreter::allow_relative_paths::HasAllowRelativePaths;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_interpreter_for_build::interpreter::global_interpreter_state::HasGlobalInterpreterState;
use buck2_interpreter_for_build::interpreter::interpreter_for_dir::ParseData;
use buck2_server_ctx::commands::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::ctx::ServerCommandDiceContext;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use buck2_server_ctx::streaming_request_handler::StreamingRequestHandler;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::FutureExt;
use futures::SinkExt;
use futures::StreamExt;
use futures::channel::mpsc::UnboundedSender;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_types::Url;
use starlark::analysis::find_call_name::AstModuleFindCallName;
use starlark::codemap::Span;
use starlark::docs::DocModule;
use starlark::docs::markdown::render_doc_item_no_link;
use starlark::errors::EvalMessage;
use starlark::syntax::AstModule;
use starlark_lsp::error::eval_message_to_lsp_diagnostic;
use starlark_lsp::server::LspContext;
use starlark_lsp::server::LspEvalResult;
use starlark_lsp::server::LspUrl;
use starlark_lsp::server::StringLiteralResult;
use starlark_lsp::server::server_with_connection;
use tokio::runtime::Handle;

/// Errors when [`LspContext::resolve_load()`] cannot resolve a given path.
#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum ResolveLoadError {
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

/// Construct a `starlark:` URL for a native builtin symbol.
fn native_starlark_url(sym: &str) -> buck2_error::Result<LspUrl> {
    let filename = Path::new(sym);
    let filename = filename.with_extension(match filename.extension() {
        None => "bzl".to_owned(),
        Some(e) => format!("{}.bzl", e.to_str().expect("path is UTF-8")),
    });
    let path = Path::new("/native").join(filename);
    LspUrl::try_from(
        Url::parse(&format!("starlark:{}", path.display()))
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?,
    )
    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))
}

/// Construct a `file:` URL for the prelude import path.
fn prelude_uri(
    location: &ImportPath,
    fs: &ProjectRoot,
    cell_resolver: &CellResolver,
) -> buck2_error::Result<LspUrl> {
    let relative_path = cell_resolver.resolve_path(location.path().as_ref())?;
    let abs_path = fs.resolve(&relative_path);
    Url::from_file_path(abs_path)
        .unwrap()
        .try_into()
        .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))
}

struct BuckLspContext<'a> {
    server_ctx: &'a dyn ServerCommandContextTrait,
    fs: ProjectRoot,
    runtime: Handle,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum BuckLspContextError {
    /// The scheme provided was not correct or supported.
    #[error("Url `{}` was expected to be of type `{}`", .1, .0)]
    WrongScheme(String, LspUrl),
}

impl<'a> BuckLspContext<'a> {
    async fn new(
        server_ctx: &'a dyn ServerCommandContextTrait,
    ) -> buck2_error::Result<BuckLspContext<'a>> {
        let fs = server_ctx
            .with_dice_ctx(
                |server_ctx, _dice_ctx| async move { Ok(server_ctx.project_root().clone()) },
            )
            .await?;

        Ok(Self {
            server_ctx,
            fs,
            runtime: Handle::current(),
        })
    }

    // We are purposefully only holding the DICE context briefly since the LSP logic lives in a
    // long-running thread, and we don't want it to block continuously while holding DICE since
    // it will block other concurrent commands.
    async fn with_dice_ctx<F, Fut, R>(&self, exec: F) -> buck2_error::Result<R>
    where
        F: FnOnce(DiceTransaction) -> Fut + Send,
        Fut: Future<Output = buck2_error::Result<R>> + Send,
        R: Send,
    {
        self.server_ctx
            .with_dice_ctx(|_, dice_ctx| async move { Ok(exec(dice_ctx).await) })
            .await?
    }

    /// A user has an LSP file open, we need a starlark path for it.
    async fn resolve_lsp_file_path(&self, path: &Path) -> buck2_error::Result<OwnedStarlarkPath> {
        let abs_path = AbsPath::new(path)?;
        let relative_path = self.fs.relativize_any(abs_path)?;
        self.with_dice_ctx(|mut dice_ctx| async move {
            let cell_resolver = dice_ctx.get_cell_resolver().await?;

            let cell_path = cell_resolver.get_cell_path(&relative_path);
            let buildfile_names = dice_ctx.get_buildfiles(cell_path.cell()).await?;

            if path.extension().is_some_and(|e| e == "bxl") {
                return Ok(OwnedStarlarkPath::BxlFile(BxlFilePath::new(cell_path)?));
            }

            let file_name = path.file_name().and_then(|x| x.to_str());
            let matching_buildfile_name = buildfile_names
                .iter()
                .find(|name| file_name.is_some_and(|f| f == name.as_str()));

            if let Some(buildfile_name) = matching_buildfile_name {
                let parent = cell_path
                    .as_ref()
                    .parent()
                    .expect("It has a filename, therefore it has a parent dir");
                let package_label = PackageLabel::from_cell_path(parent)?;
                return Ok(OwnedStarlarkPath::BuildFile(BuildFilePath::new(
                    package_label,
                    buildfile_name.clone(),
                )));
            }

            if let Some(pfp) = PackageFilePath::from_file_path(cell_path.as_ref()) {
                return Ok(OwnedStarlarkPath::PackageFile(pfp));
            }

            // Instantiating a BuildFileCell off of the path of the file being originally evaluated.
            // Unlike the build commands and such, there is no meaningful "build file cell" versus
            // the cell of the file being currently evaluated.
            let bfc = BuildFileCell::new(cell_path.cell());

            Ok(OwnedStarlarkPath::LoadFile(ImportPath::new_hack_for_lsp(
                cell_path, bfc,
            )?))
        })
        .await
    }

    /// starlark:path/into/project -> a fake starlark path
    ///
    /// `starlark:` URIs are used for markdown doc rendering, e.g. for builtins.
    /// But we do currently set their filetype to starlark and they do start
    /// hitting the LSP with requests. Ideally those docs would have the correct
    /// filetype.
    async fn resolve_fake_starlark_path(
        &self,
        path: &Path,
    ) -> buck2_error::Result<OwnedStarlarkPath> {
        // The "absolute" path from LSP urls doesn't work here, they have to be relative
        // to get a ProjectRelativePath. We already guaranteed that things start with a '/'
        // (rooted from `starlark:`, see LspUrl), so just drop it.
        let cell_resolver = self
            .with_dice_ctx(|mut dice_ctx| async move { dice_ctx.get_cell_resolver().await })
            .await?;

        let path_str = path
            .to_str()
            .ok_or_else(|| internal_error!("Path is not UTF-8"))?;

        let cell_path = cell_resolver.get_cell_path(&ProjectRelativePath::new(
            path_str.strip_prefix('/').unwrap_or(path_str),
        )?);

        match path.extension() {
            Some(e) if e == "bxl" => Ok(OwnedStarlarkModulePath::BxlFile(BxlFilePath::new(
                cell_path,
            )?)),
            _ => Ok(OwnedStarlarkModulePath::LoadFile(
                ImportPath::new_same_cell(cell_path)?,
            )),
        }
        .map(|mod_path| mod_path.into_starlark_path())
    }

    async fn resolve_lsp_uri(&self, uri: &LspUrl) -> buck2_error::Result<OwnedStarlarkPath> {
        match uri {
            LspUrl::File(path) => self.resolve_lsp_file_path(path).await,
            LspUrl::Starlark(path) => self.resolve_fake_starlark_path(path).await,
            LspUrl::Other(_) => Err(BuckLspContextError::WrongScheme(
                "file:// or starlark:".to_owned(),
                uri.clone(),
            )
            .into()),
        }
    }

    async fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult {
        match self
            .parse_file_from_contents_and_handle_diagnostic(uri, content)
            .await
        {
            Ok(res) => res,
            Err(e) => {
                let message = EvalMessage::from_any_error(uri.path(), &e);
                LspEvalResult {
                    diagnostics: vec![eval_message_to_lsp_diagnostic(message)],
                    ast: None,
                }
            }
        }
    }

    async fn parse_file_from_contents_and_handle_diagnostic(
        &self,
        uri: &LspUrl,
        content: String,
    ) -> buck2_error::Result<LspEvalResult> {
        let starlark_path = self.resolve_lsp_uri(uri).await?;

        self.with_dice_ctx(|mut dice_ctx| async move {
            let calculator = dice_ctx
                .get_interpreter_calculator(starlark_path.clone())
                .await?;

            let parse_result =
                calculator.prepare_eval_with_content(starlark_path.borrow(), content)?;
            match parse_result {
                Ok(ParseData(ast, _)) => Ok(LspEvalResult {
                    diagnostics: Vec::new(),
                    ast: Some(ast),
                }),
                Err(e) => {
                    let message = EvalMessage::from_error(uri.path(), &e.into());
                    Ok(LspEvalResult {
                        diagnostics: vec![eval_message_to_lsp_diagnostic(message)],
                        ast: None,
                    })
                }
            }
        })
        .await
    }

    async fn parse_file_from_string(
        &self,
        current_package: CellPathRef<'_>,
        literal: &str,
    ) -> buck2_error::Result<Option<StringLiteralResult>> {
        match ForwardRelativePath::new(literal) {
            Ok(package_relative) => {
                let cell_resolver = self
                    .with_dice_ctx(|mut dice_ctx| async move { dice_ctx.get_cell_resolver().await })
                    .await?;
                let relative_path =
                    cell_resolver.resolve_path(current_package.join(package_relative).as_ref())?;

                let path = self.fs.resolve(&relative_path);
                let url = Url::from_file_path(path)
                    .unwrap()
                    .try_into()
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?;
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
        current_package: CellPathRef<'_>,
        literal: &str,
    ) -> buck2_error::Result<Option<StringLiteralResult>> {
        let (artifact_fs, cell_alias_resolver, dir_with_allowed_relative_dirs) = self
            .with_dice_ctx(|mut dice_ctx| async move {
                Ok((
                    dice_ctx.get_artifact_fs().await?,
                    dice_ctx
                        .get_cell_alias_resolver(current_package.cell())
                        .await?,
                    dice_ctx
                        .dirs_allowing_relative_paths(current_package.to_owned())
                        .await?,
                ))
            })
            .await?;
        let cell_resolver = artifact_fs.cell_resolver();
        match ParsedPattern::<ProvidersPatternExtra>::parse_not_relaxed(
            literal,
            if dir_with_allowed_relative_dirs.has_allowed_relative_dir() {
                TargetParsingRel::AllowRelative(&dir_with_allowed_relative_dirs, None)
            } else {
                TargetParsingRel::AllowLimitedRelative(current_package)
            },
            cell_resolver,
            &cell_alias_resolver,
        ) {
            Ok(ParsedPattern::Target(package, target, _)) => {
                let res = self
                    .with_dice_ctx(|mut dice_ctx| async move {
                        Ok(DicePackageListingResolver(&mut dice_ctx)
                            .resolve_package_listing(package.dupe())
                            .await
                            .and_then(|listing| {
                                // In the case of external cells, we need to actually materialize
                                // this thing on disk, so treat it like a source artifact
                                let buildfile: &PackageRelativePath = listing.buildfile().as_ref();
                                let source_path = SourcePath::new(package.dupe(), buildfile.into());
                                let relative_path =
                                    artifact_fs.resolve_source(source_path.as_ref())?;
                                let path = self.fs.resolve(&relative_path);
                                match Url::from_file_path(path).unwrap().try_into() {
                                    Ok(url) => {
                                        let string_literal = StringLiteralResult {
                                            url,
                                            location_finder: Some(Box::new(|ast| {
                                                Ok(Self::find_target(ast, target))
                                            })),
                                        };
                                        Ok(Some(string_literal))
                                    }
                                    Err(e) => Err(from_any_with_tag(e, buck2_error::ErrorTag::Lsp)),
                                }
                            }))
                    })
                    .await?;
                Ok(res?)
            }
            _ => Ok(None),
        }
    }

    fn find_target(ast: &AstModule, target: TargetName) -> Option<Span> {
        ast.find_function_call_with_name(target.as_str())
    }
}

impl LspContext for BuckLspContext<'_> {
    fn parse_file_with_contents(&self, uri: &LspUrl, content: String) -> LspEvalResult {
        let dispatcher = self.server_ctx.events().dupe();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                match uri {
                    LspUrl::File(_) | LspUrl::Starlark(_) => {
                        self.parse_file_with_contents(uri, content).await
                    }
                    _ => LspEvalResult::default(),
                }
            }))
    }

    fn resolve_load(
        &self,
        path: &str,
        current_file: &LspUrl,
        _workspace_root: Option<&Path>,
    ) -> Result<LspUrl, String> {
        let dispatcher = self.server_ctx.events().dupe();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                match current_file {
                    LspUrl::File(current_file) => {
                        let current_starlark_path =
                            &self.resolve_lsp_file_path(current_file).await?;
                        let url = self
                            .with_dice_ctx(|mut dice_ctx| async move {
                                let calculator = dice_ctx
                                    .get_interpreter_calculator(current_starlark_path.clone())
                                    .await?;

                                let loaded_import_path = calculator
                                    .resolve_load(current_starlark_path.borrow(), path)
                                    .await?;
                                let relative_path = dice_ctx
                                    .get_cell_resolver()
                                    .await?
                                    .resolve_path(loaded_import_path.borrow().path().as_ref())?;
                                let abs_path = self.fs.resolve(&relative_path);
                                Url::from_file_path(abs_path)
                                    .unwrap()
                                    .try_into()
                                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))
                            })
                            .await?;

                        buck2_error::Ok(url)
                    }
                    _ => Err(ResolveLoadError::WrongScheme(
                        "file://".to_owned(),
                        current_file.clone(),
                    )
                    .into()),
                }
            }))
            .map_err(|e| format!("{:#}", e))
    }

    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &LspUrl,
        _workspace_root: Option<&Path>,
    ) -> Result<Option<StringLiteralResult>, String> {
        let dispatcher = self.server_ctx.events().dupe();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                let starlark_path = match current_file {
                    LspUrl::File(current_file) => Ok(self
                        .resolve_lsp_file_path(current_file.parent().unwrap())
                        .await?),
                    _ => Err(ResolveLoadError::WrongScheme(
                        "file://".to_owned(),
                        current_file.clone(),
                    )),
                }
                .map_err(buck2_error::Error::from)?;

                let current_package = starlark_path.borrow().path();
                let current_package_ref = current_package.as_ref().as_ref();

                // Right now we swallow the errors up as they can happen for a lot of reasons that are
                // perfectly recoverable (e.g. an invalid cell is specified, we can't list an invalid
                // package path, an absolute path is requested, etc).
                //
                // buck2_error::Error sort of obscures the root causes, so we just have
                // to assume if it failed, it's fine, and we can maybe try the next thing.
                if let Ok(Some(string_literal)) = self
                    .parse_target_from_string(current_package_ref, literal)
                    .await
                {
                    Ok(Some(string_literal))
                } else if let Ok(Some(string_literal)) = self
                    .parse_file_from_string(current_package_ref, literal)
                    .await
                {
                    Ok(Some(string_literal))
                } else {
                    buck2_error::Ok(None)
                }
            }))
            .map_err(|e| format!("{:#}", e))
    }

    fn get_load_contents(&self, uri: &LspUrl) -> Result<Option<String>, String> {
        let dispatcher = self.server_ctx.events().dupe();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                match uri {
                    LspUrl::File(path) => {
                        let path = self.resolve_lsp_file_path(path).await?;

                        self.with_dice_ctx(|mut dice_ctx| async move {
                            DiceFileComputations::read_file_if_exists(
                                &mut dice_ctx,
                                path.borrow().path().as_ref().as_ref(),
                            )
                            .await
                        })
                        .await
                    }
                    LspUrl::Starlark(_) => {
                        // Look up the symbol name from the starlark: URL and
                        // render its docs as starlark source on the fly.
                        self.with_dice_ctx(|mut dice_ctx| async move {
                            let globals_docs = dice_ctx
                                .get_global_interpreter_state()
                                .await?
                                .globals()
                                .documentation();
                            for (sym, mem) in &globals_docs.members {
                                let sym_url = native_starlark_url(sym)?;
                                if &sym_url == uri {
                                    return Ok(Some(render_doc_item_no_link(sym, mem)));
                                }
                            }
                            buck2_error::Ok(None)
                        })
                        .await
                    }
                    _ => Err(
                        BuckLspContextError::WrongScheme("file://".to_owned(), uri.clone()).into(),
                    ),
                }
            }))
            .map_err(|e| format!("{:#}", e))
    }

    fn get_url_for_global_symbol(
        &self,
        _current_file: &LspUrl,
        symbol: &str,
    ) -> Result<Option<LspUrl>, String> {
        let dispatcher = self.server_ctx.events().dupe();
        let symbol = symbol.to_owned();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                self.with_dice_ctx(|mut dice_ctx| async move {
                    let global_state = dice_ctx.get_global_interpreter_state().await?;
                    let globals_docs = global_state.globals().documentation();

                    // Check native builtins first
                    if globals_docs.members.contains_key(&symbol) {
                        return Ok(Some(native_starlark_url(&symbol)?));
                    }

                    // Check prelude symbols
                    if let Some(prelude_path) = global_state.configuror.prelude_import() {
                        let cell_resolver = dice_ctx.get_cell_resolver().await?;
                        let prelude_url =
                            prelude_uri(prelude_path.import_path(), &self.fs, &cell_resolver)?;
                        if let Ok(prelude_module) = dice_ctx
                            .get_loaded_module(StarlarkModulePath::LoadFile(
                                prelude_path.import_path(),
                            ))
                            .await
                        {
                            let prelude_docs = prelude_module.env().documentation();
                            if prelude_docs.members.contains_key(&symbol) {
                                return Ok(Some(prelude_url));
                            }
                        }
                    }

                    buck2_error::Ok(None)
                })
                .await
            }))
            .map_err(|e| format!("{:#}", e))
    }

    fn render_as_load(
        &self,
        _target: &LspUrl,
        _current_file: &LspUrl,
        _workspace_root: Option<&Path>,
    ) -> Result<String, String> {
        Err("Not yet implemented, render_as_load".to_owned())
    }

    fn get_environment(&self, url: &LspUrl) -> DocModule {
        let dispatcher = self.server_ctx.events().dupe();
        self.runtime
            .block_on(with_dispatcher_async(dispatcher, async {
                let Ok(starlark_path) = self.resolve_lsp_uri(url).await else {
                    return Ok(DocModule::default());
                };

                self.with_dice_ctx(|mut dice_ctx| async move {
                    let docs = dice_ctx.get_doc_environment(starlark_path.borrow()).await?;
                    buck2_error::Ok((*docs).clone())
                })
                .await
            }))
            .unwrap_or_default()
    }
}

pub(crate) async fn run_lsp_server_command(
    ctx: &dyn ServerCommandContextTrait,
    partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::LspMessage>,
    req: StreamingRequestHandler<buck2_cli_proto::LspRequest>,
) -> buck2_error::Result<buck2_cli_proto::LspResponse> {
    let start_event = ctx
        .command_start_event(buck2_data::LspCommandStart {}.into())
        .await?;
    span_async(start_event, async move {
        let result = run_lsp_server(ctx, partial_result_dispatcher, req).await;
        let end_event = command_end(&result, buck2_data::LspCommandEnd {});
        (result, end_event)
    })
    .await
}

/// Run an LSP server for a given client.
async fn run_lsp_server(
    ctx: &dyn ServerCommandContextTrait,
    mut partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::LspMessage>,
    mut req: StreamingRequestHandler<LspRequest>,
) -> buck2_error::Result<LspResponse> {
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

    let dispatcher = ctx.events().dupe();
    let buck_lsp_ctx = BuckLspContext::new(ctx).await?;

    tokio::task::block_in_place(|| {
        thread::scope(|scope| {
            let recv_thread = scope.spawn(move || {
                let runtime = tokio::runtime::Builder::new_current_thread()
                    .build()
                    .unwrap();
                runtime.block_on(recv_from_lsp(client_receiver, events_from_server))
            });

            let server_thread = scope.spawn(with_dispatcher(dispatcher, || {
                move || server_with_connection(connection, buck_lsp_ctx)
            }));

            let res = {
                let runtime = tokio::runtime::Builder::new_current_thread()
                    .build()
                    .unwrap();

                runtime.block_on(async move {
                    loop {
                        let message_handler_res = tokio::select! {
                            m = req.message().fuse() => {
                                handle_incoming_lsp_message(&send_to_server, m)
                            },
                            m = events_to_client.next() => {
                                Ok(handle_outgoing_lsp_message(&mut partial_result_dispatcher, m))
                            },
                        };
                        match message_handler_res {
                            Ok(Some(res)) => break Ok(res),
                            Ok(None) => {}
                            Err(e) => break Err(e),
                        }
                    }
                })
            };

            let _ignored = recv_thread.join();
            let _ignored = server_thread.join();
            res
        })
    })
}

/// Receive messages from the LSP's channel, and pass them to the client after encapsulating them.
///
/// This returns `Ok(())` when the other end of the connection disconnects.
async fn recv_from_lsp(
    to_client: crossbeam_channel::Receiver<Message>,
    mut event_sender: UnboundedSender<buck2_cli_proto::LspMessage>,
) -> buck2_error::Result<()> {
    loop {
        let msg = to_client
            .recv()
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?;

        let lsp_json = serde_json::to_string(&msg).unwrap();
        let res = buck2_cli_proto::LspMessage { lsp_json };
        match event_sender.send(res).await {
            Ok(_) => {}
            Err(e) if e.is_disconnected() => break Ok(()),
            Err(e) => break Err(from_any_with_tag(e, buck2_error::ErrorTag::Lsp)),
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
    message: buck2_error::Result<LspRequest>,
) -> buck2_error::Result<Option<LspResponse>> {
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
    partial_result_dispatcher: &mut PartialResultDispatcher<buck2_cli_proto::LspMessage>,
    event: Option<buck2_cli_proto::LspMessage>,
) -> Option<LspResponse> {
    match event {
        Some(event) => {
            partial_result_dispatcher.emit(event);
            None
        }
        None => Some(LspResponse {}),
    }
}

#[cfg(test)]
mod tests {
    use buck2_error::conversion::from_any_with_tag;
    use lsp_types::Url;
    use starlark_lsp::server::LspUrl;

    use crate::lsp::native_starlark_url;

    #[test]
    fn native_starlark_url_construction() -> buck2_error::Result<()> {
        assert_eq!(
            LspUrl::try_from(
                Url::parse("starlark:/native/native_function1.bzl")
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?,
            native_starlark_url("native_function1")?
        );
        assert_eq!(
            LspUrl::try_from(
                Url::parse("starlark:/native/native_function2.bzl")
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?
            )
            .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Lsp))?,
            native_starlark_url("native_function2")?
        );
        Ok(())
    }
}
