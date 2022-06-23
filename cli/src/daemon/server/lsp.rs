/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::io::ErrorKind;
use std::path::Path;

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::pattern::ParsedPattern;
use buck2_common::pattern::ProvidersPattern;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::package::Package;
use buck2_core::result::SharedResult;
use buck2_core::target::TargetName;
use buck2_interpreter::common::BuildFileCell;
use buck2_interpreter::common::ImportPath;
use buck2_interpreter::common::StarlarkPath;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::package_listing::dice::HasPackageListingResolver;
use buck2_interpreter::package_listing::resolver::PackageListingResolver;
use cli_proto::*;
use dice::DiceTransaction;
use events::dispatch::EventDispatcher;
use futures::channel::mpsc::UnboundedSender;
use futures::FutureExt;
use futures::SinkExt;
use futures::StreamExt;
use lsp_server::Connection;
use lsp_server::Message;
use lsp_types::Range;
use lsp_types::Url;
use starlark::errors::EvalMessage;
use starlark::lsp::server::server_with_connection;
use starlark::lsp::server::LspContext;
use starlark::lsp::server::LspEvalResult;
use starlark::lsp::server::StringLiteralResult;
use starlark::syntax::AstModule;
use tonic::Status;

use crate::daemon::server::ServerCommandContext;
use crate::daemon::server::StreamingRequestHandler;

struct BuckLspContext {
    dice_ctx: DiceTransaction,
    fs: ProjectFilesystem,
    cell_resolver: CellResolver,
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

    fn runtime(&self) -> tokio::runtime::Runtime {
        tokio::runtime::Builder::new_current_thread()
            .build()
            .unwrap()
    }

    async fn parse_file_with_contents(
        &self,
        uri: &Url,
        content: String,
    ) -> SharedResult<LspEvalResult> {
        let import_path = self.import_path(Path::new(uri.path()))?;
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
                let relative_path = self.cell_resolver.resolve_path(
                    current_package
                        .join_unnormalized(package_relative)
                        .as_cell_path(),
                )?;

                let path = self.fs.resolve(&relative_path);
                let url = Url::from_file_path(path).unwrap();
                let string_literal = StringLiteralResult {
                    url,
                    location_finder: box |_ast, _url| Ok(None),
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
                            .join_unnormalized(listing.buildfile());
                        let path = self.fs.resolve(&relative_path);
                        let url = Url::from_file_path(path).unwrap();
                        let string_literal = StringLiteralResult {
                            url,
                            location_finder: box |ast, _url| Ok(Self::find_target(ast, target)),
                        };
                        Ok(Some(string_literal))
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
    fn parse_file_with_contents(&self, uri: &Url, content: String) -> LspEvalResult {
        self.runtime().block_on(async {
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
        })
    }

    fn resolve_load(&self, path: &str, current_file: &Path) -> anyhow::Result<Url> {
        self.runtime().block_on(async {
            let current_import_path = self.import_path(current_file)?;
            let calculator = self
                .dice_ctx
                .get_interpreter_calculator(
                    current_import_path.cell(),
                    current_import_path.build_file_cell(),
                )
                .await?;

            let starlark_file = StarlarkPath::LoadFile(&current_import_path);
            let loaded_import_path = calculator.resolve_load(starlark_file, path).await?;
            let relative_path = self.cell_resolver.resolve_path(loaded_import_path.path())?;
            let abs_path = self.fs.resolve(&relative_path);
            let url = Url::from_file_path(abs_path).unwrap();

            Ok(url)
        })
    }

    fn resolve_string_literal(
        &self,
        literal: &str,
        current_file: &Path,
    ) -> anyhow::Result<Option<StringLiteralResult>> {
        let import_path = self.import_path(current_file.parent().unwrap())?;
        let current_package = Package::from_cell_path(import_path.path());
        let runtime = self.runtime();
        runtime.block_on(async move {
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
        })
    }

    fn get_load_contents(&self, uri: &Url) -> anyhow::Result<Option<String>> {
        self.runtime().block_on(async {
            let path = self.import_path(Path::new(uri.path()))?;
            match self.dice_ctx.file_ops().read_file(path.path()).await {
                Ok(s) => Ok(Some(s)),
                Err(e) => match e.downcast_ref::<io::Error>() {
                    Some(inner_e) if inner_e.kind() == ErrorKind::NotFound => Ok(None),
                    _ => Err(e),
                },
            }
        })
    }
}

/// Run an LSP server for a given client.
pub(crate) async fn run_lsp_server(
    ctx: ServerCommandContext,
    mut req: StreamingRequestHandler<LspRequest>,
) -> anyhow::Result<LspResponse> {
    let dice_ctx: DiceTransaction = ctx.dice_ctx().await?;
    let cell_resolver: CellResolver = dice_ctx.get_cell_resolver().await?;
    let fs = ctx.file_system();

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

    let server_thread = std::thread::spawn(move || {
        server_with_connection(
            connection,
            BuckLspContext {
                dice_ctx,
                fs,
                cell_resolver,
            },
        )
    });

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
