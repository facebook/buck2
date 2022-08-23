/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::commands::streaming::StreamingCommand;
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_client::common::ConsoleType;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::exit_result::ExitResult;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use cli_proto::ClientContext;
use cli_proto::GenericRequest;
use futures::FutureExt;

use crate::commands::audit::analysis_queries::AuditAnalysisQueriesCommand;
use crate::commands::audit::cell::AuditCellCommand;
use crate::commands::audit::config::AuditConfigCommand;
use crate::commands::audit::configurations::AuditConfigurationsCommand;
use crate::commands::audit::dep_files::AuditDepFilesCommand;
use crate::commands::audit::execution_platform_resolution::AuditExecutionPlatformResolutionCommand;
use crate::commands::audit::includes::AuditIncludesCommand;
use crate::commands::audit::prelude::AuditPreludeCommand;
use crate::commands::audit::providers::AuditProvidersCommand;
use crate::commands::audit::starlark::StarlarkCommand;
use crate::commands::audit::visibility::AuditVisibilityCommand;

pub mod analysis_queries;
pub mod cell;
pub mod config;
pub mod configurations;
pub mod dep_files;
pub mod execution_platform_resolution;
pub mod includes;
pub mod prelude;
pub mod providers;
pub(crate) mod server;
pub mod starlark;
pub mod visibility;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "audit", about = "Perform lower level queries")]
pub(crate) enum AuditCommand {
    Cell(AuditCellCommand),
    Config(AuditConfigCommand),
    Configurations(AuditConfigurationsCommand),
    Includes(AuditIncludesCommand),
    Prelude(AuditPreludeCommand),
    Providers(AuditProvidersCommand),
    AnalysisQueries(AuditAnalysisQueriesCommand),
    ExecutionPlatformResolution(AuditExecutionPlatformResolutionCommand),
    Visibility(AuditVisibilityCommand),
    #[clap(subcommand)]
    Starlark(StarlarkCommand),
    DepFiles(AuditDepFilesCommand),
}

/// `buck2 audit` subcommands have a somewhat unique approach to make it really easy to
/// add them without the boilerplate necessary for normal commands. The main difference
/// is that there is not a custom endpoint added in the daemon for each subcommand, instead
/// there is a single endpoint where we send the entire serialized AuditCommand and
/// reproduce it in the daemon.
///
/// Audit subcommands implement this trait so that we can handle the entire client side
/// logic here and to support that serialization to the daemon.
#[async_trait]
pub(crate) trait AuditSubcommand: Send + Sync + 'static {
    async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()>;

    fn config_opts(&self) -> Option<&CommonBuildConfigurationOptions>;

    fn console_opts(&self) -> Option<&CommonConsoleOptions>;

    fn event_log_opts(&self) -> Option<&CommonDaemonCommandOptions>;
}

impl AuditCommand {
    pub(crate) async fn server_execute(
        &self,
        server_ctx: Box<dyn ServerCommandContextTrait>,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()> {
        self.as_subcommand()
            .server_execute(server_ctx, client_server_ctx)
            .await
    }
    fn as_subcommand(&self) -> &dyn AuditSubcommand {
        match self {
            AuditCommand::Cell(cmd) => cmd,
            AuditCommand::Config(cmd) => cmd,
            AuditCommand::Configurations(cmd) => cmd,
            AuditCommand::Includes(cmd) => cmd,
            AuditCommand::Prelude(cmd) => cmd,
            AuditCommand::Providers(cmd) => cmd,
            AuditCommand::AnalysisQueries(cmd) => cmd,
            AuditCommand::ExecutionPlatformResolution(cmd) => cmd,
            AuditCommand::Starlark(cmd) => cmd,
            AuditCommand::DepFiles(cmd) => cmd,
            AuditCommand::Visibility(cmd) => cmd,
        }
    }
}

#[async_trait]
impl StreamingCommand for AuditCommand {
    const COMMAND_NAME: &'static str = "audit";

    /// Audit subcommands are all implemented as a generic request to the buckd server that will deserialize the command object.
    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        server_ctx: ClientCommandContext,
    ) -> ExitResult {
        let serialized = serde_json::to_string(&self)?;

        let config_opts = self.as_subcommand().config_opts();

        let submatches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        let context = match config_opts {
            Some(opts) => server_ctx.client_context(opts, submatches)?,
            _ => server_ctx.empty_client_context()?,
        };

        buckd
            .with_flushing(|client| {
                client
                    .audit(GenericRequest {
                        context: Some(context),
                        serialized_opts: serialized,
                    })
                    .boxed()
            })
            .await???;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static DEFAULT_OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::Simple,
            ui: vec![],
        };
        match self.as_subcommand().console_opts() {
            Some(v) => v,
            None => &DEFAULT_OPTS,
        }
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        static DEFAULT_OPTS: CommonDaemonCommandOptions = CommonDaemonCommandOptions {
            no_event_log: false,
            event_log: None,
            build_id_file: None,
        };

        match self.as_subcommand().event_log_opts() {
            Some(v) => v,
            None => &DEFAULT_OPTS,
        }
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        match self.as_subcommand().config_opts() {
            Some(v) => v,
            None => CommonBuildConfigurationOptions::default_ref(),
        }
    }
}
