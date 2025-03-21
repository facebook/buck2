/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::ConfiguredTargetsRequest;
use buck2_cli_proto::ConfiguredTargetsResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;

/// Resolve target patterns to configured targets.
#[derive(Debug, clap::Parser)]
#[clap(name = "ctargets")]
pub struct ConfiguredTargetsCommand {
    /// Skip missing targets from `BUCK` files when non-glob pattern is specified.
    /// This option does not skip missing packages
    /// and does not ignore errors of `BUCK` file evaluation.
    #[clap(long)]
    skip_missing_targets: bool,

    /// Patterns to interpret.
    #[clap(name = "TARGET_PATTERNS", value_hint = clap::ValueHint::Other)]
    patterns: Vec<String>,

    #[clap(flatten)]
    target_cfg: TargetCfgOptions,

    #[clap(flatten)]
    common_opts: CommonCommandOptions,
}

#[async_trait]
impl StreamingCommand for ConfiguredTargetsCommand {
    const COMMAND_NAME: &'static str = "ctargets";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let context = Some(ctx.client_context(matches, &self)?);
        let ConfiguredTargetsResponse {
            serialized_targets_output,
        } = buckd
            .with_flushing()
            .ctargets(
                ConfiguredTargetsRequest {
                    context,
                    target_patterns: self.patterns,
                    target_cfg: Some(self.target_cfg.target_cfg()),
                    skip_missing_targets: self.skip_missing_targets,
                },
                events_ctx,
                ctx.console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await??;

        buck2_client_ctx::print!("{}", serialized_targets_output)?;

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
