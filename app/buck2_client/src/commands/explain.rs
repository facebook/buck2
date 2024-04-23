/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::ExplainRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;
use tonic::async_trait;

/// Buck2 Explain
///
/// This command is to allow users to dive in and understand
/// builds, without requiring a solid grasp of Buck2 concepts
#[derive(Debug, clap::Parser)]
#[clap(name = "explain")]
pub struct ExplainCommand {
    /// Output file path for profile data.
    ///
    /// File will be created if it does not exist, and overwritten if it does.
    #[clap(long, short = 'o')]
    output: PathArg,
    // TODO iguridi: pass target for now, eventually read it from the logs for an actual build
    /// Target to get information from
    #[clap(long, short = 't')]
    target: String,
}

// TODO: not sure I need StreamingCommand
#[async_trait]
impl StreamingCommand for ExplainCommand {
    const COMMAND_NAME: &'static str = "explain";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let output = self.output.resolve(&ctx.working_dir);

        if !cfg!(windows) {
            let context = ctx.client_context(matches, &self)?;
            buckd
                .with_flushing()
                .new_generic(
                    context,
                    NewGenericRequest::Explain(ExplainRequest {
                        output,
                        target: self.target,
                    }),
                    None,
                )
                .await??;

            ExitResult::success()
        } else {
            ExitResult::bail("Not implemented for windows")
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::default_ref()
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        CommonStarlarkOptions::default_ref()
    }
}
