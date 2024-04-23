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
use buck2_cli_proto::GenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::StdoutPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use classpath::AuditClasspathCommand;

use crate::analysis_queries::AuditAnalysisQueriesCommand;
use crate::cell::AuditCellCommand;
use crate::config::AuditConfigCommand;
use crate::configurations::AuditConfigurationsCommand;
use crate::deferred_materializer::DeferredMaterializerCommand;
use crate::dep_files::AuditDepFilesCommand;
use crate::execution_platform_resolution::AuditExecutionPlatformResolutionCommand;
use crate::includes::AuditIncludesCommand;
use crate::output::command::AuditOutputCommand;
use crate::output::parse::AuditParseCommand;
use crate::package_values::PackageValuesCommand;
use crate::prelude::AuditPreludeCommand;
use crate::providers::AuditProvidersCommand;
use crate::starlark::StarlarkCommand;
use crate::subtargets::AuditSubtargetsCommand;
use crate::visibility::AuditVisibilityCommand;

pub mod analysis_queries;
pub mod cell;
pub mod classpath;
pub mod config;
pub mod configurations;
pub mod deferred_materializer;
pub mod dep_files;
pub mod execution_platform_resolution;
pub mod includes;
pub mod output;
pub mod package_values;
pub mod prelude;
pub mod providers;
pub mod starlark;
pub mod subtargets;
pub mod visibility;

#[derive(Debug, clap::Subcommand, serde::Serialize, serde::Deserialize)]
#[clap(name = "audit", about = "Perform lower level queries")]
pub enum AuditCommand {
    Cell(AuditCellCommand),
    Classpath(AuditClasspathCommand),
    Config(AuditConfigCommand),
    Configurations(AuditConfigurationsCommand),
    Includes(AuditIncludesCommand),
    Prelude(AuditPreludeCommand),
    Providers(AuditProvidersCommand),
    Subtargets(AuditSubtargetsCommand),
    AnalysisQueries(AuditAnalysisQueriesCommand),
    ExecutionPlatformResolution(AuditExecutionPlatformResolutionCommand),
    Visibility(AuditVisibilityCommand),
    #[clap(subcommand)]
    Starlark(StarlarkCommand),
    DepFiles(AuditDepFilesCommand),
    DeferredMaterializer(DeferredMaterializerCommand),
    Output(AuditOutputCommand),
    Parse(AuditParseCommand),
    PackageValues(PackageValuesCommand),
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
    fn common_opts(&self) -> &CommonCommandOptions;
}

impl AuditCommand {
    fn as_subcommand(&self) -> &dyn AuditSubcommand {
        match self {
            AuditCommand::Cell(cmd) => cmd,
            AuditCommand::Classpath(cmd) => cmd,
            AuditCommand::Config(cmd) => cmd,
            AuditCommand::Configurations(cmd) => cmd,
            AuditCommand::Includes(cmd) => cmd,
            AuditCommand::Prelude(cmd) => cmd,
            AuditCommand::Providers(cmd) => cmd,
            AuditCommand::Subtargets(cmd) => cmd,
            AuditCommand::AnalysisQueries(cmd) => cmd,
            AuditCommand::ExecutionPlatformResolution(cmd) => cmd,
            AuditCommand::Starlark(cmd) => cmd,
            AuditCommand::DepFiles(cmd) => cmd,
            AuditCommand::DeferredMaterializer(cmd) => cmd,
            AuditCommand::Visibility(cmd) => cmd,
            AuditCommand::Output(cmd) => cmd,
            AuditCommand::Parse(cmd) => cmd,
            AuditCommand::PackageValues(cmd) => cmd,
        }
    }
}

#[async_trait]
impl StreamingCommand for AuditCommand {
    const COMMAND_NAME: &'static str = "audit";

    /// Audit subcommands are all implemented as a generic request to the buckd server that will deserialize the command object.
    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let serialized = serde_json::to_string(&self)?;

        let submatches = match matches.subcommand().map(|s| s.1) {
            Some(submatches) => submatches,
            None => panic!("Parsed a subcommand but couldn't extract subcommand argument matches"),
        };

        let context = ctx.client_context(submatches, &self)?;

        buckd
            .with_flushing()
            .audit(
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
