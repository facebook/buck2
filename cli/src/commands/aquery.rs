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
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::exit_result::ExitResult;
use cli_proto::AqueryRequest;
use futures::FutureExt;

use crate::commands::uquery::CommonQueryArgs;

/// Perform queries on the action graph (experimental).
///
/// The action graph consists of all the declared actions for a build, with dependencies
/// when one action consumes the outputs of another action.
///
/// Examples:
///
/// Print the action producing a target's default output
///
/// `buck2 aquery //java/com/example/app:amazing`
///
/// List all the commands for run actions for building a target
///
/// `buck2 aquery 'kind(run, deps("//java/com/example/app:amazing+more"))' --output-attribute=cmd`
///
/// Dynamic outputs (`ctx.actions.dynamic_output`):
///
/// Currently, aquery interacts poorly with dynamic outputs. It may return incorrect results or otherwise
/// behave unexpectedly.
#[derive(Debug, clap::Parser)]
#[clap(name = "aquery")]
pub(crate) struct AqueryCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    query_common: CommonQueryArgs,
}

#[async_trait]
impl StreamingCommand for AqueryCommand {
    const COMMAND_NAME: &'static str = "aquery";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> ExitResult {
        let (query, query_args) = self.query_common.get_query();
        let unstable_output_format = self.query_common.output_format() as i32;
        let output_attributes = self.query_common.output_attributes().to_vec();
        let ctx = ctx.client_context(&self.config_opts, matches)?;

        let response = buckd
            .with_flushing(|client| {
                client
                    .aquery(AqueryRequest {
                        query,
                        query_args,
                        context: Some(ctx),
                        output_attributes,
                        unstable_output_format,
                    })
                    .boxed()
            })
            .await???;

        for message in &response.error_messages {
            buck2_client::eprintln!("{}", message)?;
        }

        if !response.error_messages.is_empty() {
            ExitResult::failure()
        } else {
            ExitResult::success()
        }
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
