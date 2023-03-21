/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_cli_proto::UqueryRequest;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::StdoutPartialResultHandler;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_query_common::query_args::CommonQueryArgs;

/// Perform queries on the unconfigured target graph.
///
/// The unconfigured target graph consists of the targets as they are defined in the build
/// files. In this graph, each target appears exactly once and `select()`s are in the unresolved
/// form. For large queries, the unconfigured graph may be much smaller than the configured
/// graph and queries can be much more efficiently performed there.
///
/// When querying the unconfigured graph, dependencies appearing in all branches of `select()`
/// dictionaries will be treated as dependencies.
///
/// Run `buck2 docs uquery` for more documentation about the functions available in cquery
/// expressions.
///
/// Examples:
///
/// Print all the attributes of a target
///
/// `buck2 uquery //java/com/example/app:amazing --output-all-attributes
///
/// List the deps of a target (special characters in a target will require quotes):
/// `buck2 uquery 'deps("//java/com/example/app:amazing+more")'`
///
/// select() encoding:
///
/// When printed, values with `select()`s use a special json encoding.
///
/// `1 + select({"//:a": 1, "DEFAULT": 2})` will be encoded as:
///
/// `{"__type": "concat", "items": [1, {"__type": "selector", "entries": {"//:a": 1, "DEFAULT": 2}}]}`
#[derive(Debug, clap::Parser)]
#[clap(name = "uquery")]
pub struct UqueryCommand {
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
impl StreamingCommand for UqueryCommand {
    const COMMAND_NAME: &'static str = "uquery";

    async fn exec_impl(
        mut self,
        buckd: &mut BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let (query, query_args) = self.query_common.get_query();
        let unstable_output_format = self.query_common.output_format() as i32;
        let output_attributes = self.query_common.attributes.get()?;
        let context = ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;

        let response = buckd
            .with_flushing()
            .uquery(
                UqueryRequest {
                    query,
                    query_args,
                    context: Some(context),
                    output_attributes,
                    unstable_output_format,
                    target_call_stacks: self.query_common.target_call_stacks,
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
                &mut StdoutPartialResultHandler,
            )
            .await??;

        for message in &response.error_messages {
            buck2_client_ctx::eprintln!("{}", message)?;
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
