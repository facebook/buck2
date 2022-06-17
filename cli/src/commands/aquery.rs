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
use cli_proto::AqueryRequest;
use futures::FutureExt;
use structopt::{clap, StructOpt};

use crate::{
    commands::{
        common::{CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions},
        uquery::CommonQueryArgs,
    },
    daemon::client::BuckdClientConnector,
    CommandContext, StreamingCommand,
};

#[derive(Debug, StructOpt)]
#[structopt(
    name = "aquery",
    about = "provides facilities to query information about the action graph"
)]
pub struct AqueryCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(flatten)]
    query_common: CommonQueryArgs,
}

#[async_trait]
impl StreamingCommand for AqueryCommand {
    const COMMAND_NAME: &'static str = "aquery";

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
