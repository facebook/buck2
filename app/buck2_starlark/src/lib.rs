/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(try_blocks)]

use async_trait::async_trait;
use buck2_cli_proto::ClientContext;
use buck2_cli_proto::GenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::StdoutPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::BuckSubcommand;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_common::argv::Argv;
use buck2_common::argv::SanitizedArgv;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::debug::StarlarkDebugAttachCommand;
use crate::lint::StarlarkLintCommand;
use crate::typecheck::StarlarkTypecheckCommand;

mod debug;
mod lint;
pub mod server;
mod typecheck;
mod util;

#[derive(Debug, clap::Subcommand)]
#[clap(name = "starlark", about = "Run Starlark operations")]
pub enum StarlarkCommand {
    #[clap(flatten)]
    Opaque(StarlarkOpaqueCommand),
    DebugAttach(StarlarkDebugAttachCommand),
}

// Used for subcommands that follow `buck2 audit`'s "opaque" pattern where the command object is serialized
// to the daemon and deserialized there and has a `server_execute()` on the Command object itself (as opposed
// to using structured endpoints in the daemon protocol).
#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
pub enum StarlarkOpaqueCommand {
    Lint(StarlarkLintCommand),
    Typecheck(StarlarkTypecheckCommand),
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize, Default)]
pub struct StarlarkCommandCommonOptions {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    starlark_opts: CommonStarlarkOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,
}

#[async_trait]
pub trait StarlarkOpaqueSubcommand: Send + Sync + 'static {
    async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()>;

    fn common_opts(&self) -> &StarlarkCommandCommonOptions;
}

impl StarlarkOpaqueCommand {
    pub async fn server_execute(
        &self,
        server_ctx: &dyn ServerCommandContextTrait,
        stdout: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        self.as_subcommand()
            .server_execute(server_ctx, stdout, client_server_ctx)
            .await
    }
    fn as_subcommand(&self) -> &dyn StarlarkOpaqueSubcommand {
        match self {
            Self::Lint(cmd) => cmd,
            Self::Typecheck(cmd) => cmd,
        }
    }
}

#[async_trait]
impl StreamingCommand for StarlarkOpaqueCommand {
    const COMMAND_NAME: &'static str = "starlark";

    /// Starlark subcommands are all implemented as a generic request to the buckd server that will deserialize the command object.
    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let serialized = serde_json::to_string(&self)?;

        let context = ctx.client_context(matches, &self)?;

        buckd
            .with_flushing()
            .starlark(
                GenericRequest {
                    context: Some(context),
                    serialized_opts: serialized,
                },
                ctx.stdin().console_interaction_stream(self.console_opts()),
                &mut StdoutPartialResultHandler,
            )
            .await??;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.as_subcommand().common_opts().console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.as_subcommand().common_opts().event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.as_subcommand().common_opts().config_opts
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        &self.as_subcommand().common_opts().starlark_opts
    }
}

impl StarlarkCommand {
    pub fn exec(self, matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let matches = matches.subcommand().expect("subcommand not found").1;
        match self {
            StarlarkCommand::Opaque(cmd) => cmd.exec(matches, ctx),
            StarlarkCommand::DebugAttach(cmd) => cmd.exec(matches, ctx),
        }
    }

    pub fn sanitize_argv(&self, argv: Argv) -> SanitizedArgv {
        argv.no_need_to_sanitize()
    }
}
