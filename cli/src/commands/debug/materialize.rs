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
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::daemon::client::BuckdConnectOptions;
use buck2_client::exit_result::ExitResult;
use cli_proto::MaterializeRequest;
use futures::FutureExt;

use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
pub(crate) struct MaterializeCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    /// Paths to materialize, relative to project root
    #[clap(value_name = "PATH")]
    paths: Vec<String>,
}

#[async_trait]
impl StreamingCommand for MaterializeCommand {
    const COMMAND_NAME: &'static str = "materialize";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b ClientCommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> ExitResult {
        let ctx = ctx.client_context(&self.config_opts, matches)?;
        buckd
            .with_flushing(|client| {
                client
                    .materialize(MaterializeRequest {
                        context: Some(ctx),
                        paths: self.paths,
                    })
                    .boxed()
            })
            .await???;

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}
