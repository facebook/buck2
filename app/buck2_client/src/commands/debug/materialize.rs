/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::new_generic::MaterializeRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub struct MaterializeCommand {
    /// Paths to materialize, relative to project root
    #[clap(value_name = "PATH")]
    paths: Vec<String>,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

#[async_trait]
impl StreamingCommand for MaterializeCommand {
    const COMMAND_NAME: &'static str = "materialize";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::Materialize(MaterializeRequest { paths: self.paths }),
                events_ctx,
                ctx.console_interaction_stream(&self.common_opts.console_opts),
            )
            .await??;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.common_opts.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.common_opts.event_log_opts
    }

    fn build_config_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.common_opts.config_opts
    }

    fn starlark_opts(&self) -> &CommonStarlarkOptions {
        &self.common_opts.starlark_opts
    }
}
