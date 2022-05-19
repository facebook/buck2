/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::{ClientContext, GenericRequest};
use structopt::{clap, StructOpt};

use crate::{
    commands::{
        audit::{
            analysis_queries::AuditAnalysisQueriesCommand, cell::AuditCellCommand,
            config::AuditConfigCommand, configurations::AuditConfigurationsCommand,
            dep_files::AuditDepFilesCommand,
            execution_platform_resolution::AuditExecutionPlatformResolutionCommand,
            includes::AuditIncludesCommand, prelude::AuditPreludeCommand,
            providers::AuditProvidersCommand, starlark::StarlarkCommand,
        },
        common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions, ConsoleType},
    },
    daemon::{client::BuckdClient, server::ServerCommandContext},
    CommandContext, StreamingCommand,
};

pub mod analysis_queries;
pub mod cell;
pub mod config;
pub mod configurations;
pub mod dep_files;
pub mod execution_platform_resolution;
pub mod includes;
pub mod prelude;
pub mod providers;
pub mod starlark;

#[derive(Debug, StructOpt, serde::Serialize, serde::Deserialize)]
#[structopt(name = "audit", about = "Perform lower level queries")]
pub enum AuditCommand {
    Cell(AuditCellCommand),
    Config(AuditConfigCommand),
    Configurations(AuditConfigurationsCommand),
    Includes(AuditIncludesCommand),
    Prelude(AuditPreludeCommand),
    Providers(AuditProvidersCommand),
    AnalysisQueries(AuditAnalysisQueriesCommand),
    ExecutionPlatformResolution(AuditExecutionPlatformResolutionCommand),
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
pub trait AuditSubcommand: Send + Sync + 'static {
    async fn server_execute(
        &self,
        server_ctx: ServerCommandContext,
        client_server_ctx: ClientContext,
    ) -> anyhow::Result<()>;

    fn config_opts(&self) -> Option<&CommonConfigOptions>;

    fn console_opts(&self) -> Option<&CommonConsoleOptions>;

    fn event_log_opts(&self) -> Option<&CommonEventLogOptions>;
}

impl AuditCommand {
    pub async fn server_execute(
        &self,
        server_ctx: ServerCommandContext,
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
        }
    }
}

#[async_trait]
impl StreamingCommand for AuditCommand {
    const COMMAND_NAME: &'static str = "audit";

    /// Audit subcommands are all implemented as a generic request to the buckd server that will deserialize the command object.
    async fn exec_impl(
        self,
        mut buckd: BuckdClient,
        matches: &clap::ArgMatches,
        server_ctx: CommandContext,
    ) -> ExitResult {
        let serialized = serde_json::to_string(&self)?;

        let config_opts = self.as_subcommand().config_opts();

        let submatches = match matches.subcommand().1 {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        let context = match config_opts {
            Some(opts) => server_ctx.client_context(opts, submatches)?,
            _ => server_ctx.empty_client_context()?,
        };

        buckd
            .audit(GenericRequest {
                context: Some(context),
                serialized_opts: serialized,
            })
            .await??;
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

    fn event_log_opts(&self) -> &super::common::CommonEventLogOptions {
        static DEFAULT_OPTS: CommonEventLogOptions = CommonEventLogOptions {
            no_event_log: false,
            event_log: None,
        };

        match self.as_subcommand().event_log_opts() {
            Some(v) => v,
            None => &DEFAULT_OPTS,
        }
    }
}
