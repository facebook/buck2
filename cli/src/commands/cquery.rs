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
use cli_proto::CqueryRequest;
use futures::FutureExt;

use crate::commands::common::CommonConfigOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonEventLogOptions;
use crate::commands::uquery::CommonQueryArgs;
use crate::daemon::client::BuckdClientConnector;
use crate::CommandContext;
use crate::StreamingCommand;

#[derive(Debug, clap::Parser)]
#[clap(name = "cquery", about = "Query configured target graph")]
pub(crate) struct CqueryCommand {
    #[clap(flatten)]
    config_opts: CommonConfigOptions,

    #[clap(flatten)]
    #[allow(unused)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[clap(flatten)]
    query_common: CommonQueryArgs,

    #[clap(
        long,
        use_delimiter = true,
        help = "Comma separated list of targets at which to root the queryable universe.
                This is useful since targets can exist in multiple configurations. While
                this argument isn't required, it's recommended for most non-trivial queries."
    )]
    target_universe: Vec<String>,

    #[clap(
        long,
        help = "Show the providers of the query result instead of the attributes and labels"
    )]
    show_providers: bool,
}

#[async_trait]
impl StreamingCommand for CqueryCommand {
    const COMMAND_NAME: &'static str = "cquery";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let (query, query_args) = self.query_common.get_query();
        let unstable_output_format = self.query_common.output_format() as i32;
        let output_attributes = self.query_common.output_attributes().to_vec();
        let ctx = ctx.client_context(&self.config_opts, matches)?;

        let response = buckd
            .with_flushing(|client| {
                client
                    .cquery(CqueryRequest {
                        query,
                        query_args,
                        context: Some(ctx),
                        output_attributes,
                        target_universe: self.target_universe,
                        show_providers: self.show_providers,
                        unstable_output_format,
                        target_call_stacks: self.query_common.target_call_stacks,
                    })
                    .boxed()
            })
            .await???;

        for message in &response.error_messages {
            crate::eprintln!("{}", message)?;
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

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
