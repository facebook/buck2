/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::new_generic::CompleteRequest;
use buck2_cli_proto::new_generic::NewGenericRequest;
use buck2_cli_proto::new_generic::NewGenericResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::target_cfg::TargetCfgOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::common::ui::ConsoleType;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use clap::ArgMatches;

type CompleteCallback = fn(anyhow::Result<Vec<String>>) -> ExitResult;

pub(crate) struct CompleteTargetCommand {
    common_opts: CommonCommandOptions,
    target_cfg: TargetCfgOptions,

    partial_target: String,
    callback: CompleteCallback,
}

impl CompleteTargetCommand {
    pub(crate) fn new(partial_target: String, callback: CompleteCallback) -> Self {
        let common_opts = CommonCommandOptions {
            config_opts: CommonBuildConfigurationOptions {
                reuse_current_config: true,
                ..Default::default()
            },
            console_opts: CommonConsoleOptions {
                console_type: ConsoleType::None,
                ..Default::default()
            },
            ..Default::default()
        };

        let target_cfg = TargetCfgOptions::default();
        Self {
            common_opts,
            target_cfg,
            partial_target,
            callback,
        }
    }
}

#[async_trait::async_trait]
impl StreamingCommand for CompleteTargetCommand {
    const COMMAND_NAME: &'static str = "complete";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: &ArgMatches,
        ctx: &mut ClientCommandContext<'_>,
    ) -> ExitResult {
        let context = ctx.client_context(matches, &self)?;
        let res = buckd
            .with_flushing()
            .new_generic(
                context,
                NewGenericRequest::Complete(CompleteRequest {
                    target_cfg: self.target_cfg.target_cfg(),
                    partial_target: self.partial_target,
                }),
                None,
            )
            .await??;
        let NewGenericResponse::Complete(res) = res else {
            return ExitResult::bail("Unexpected response type from generic command");
        };

        (self.callback)(Ok(res.completions))
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
