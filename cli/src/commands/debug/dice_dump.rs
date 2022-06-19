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
use cli_proto::{unstable_dice_dump_request::DiceDumpFormat, UnstableDiceDumpRequest};
use futures::FutureExt;

use crate::{
    commands::common::{CommonConsoleOptions, CommonEventLogOptions, ConsoleType},
    daemon::client::{BuckdClientConnector, BuckdConnectOptions},
    CommandContext, StreamingCommand,
};

#[derive(Debug, clap::Parser)]
pub(crate) struct DiceDumpCommand {
    /// The path to write the heap dump to.
    #[clap(short, long, value_name = "PATH")]
    path: String,
    #[clap(long, group = "dice_dump_format")]
    serde: bool,
}

#[async_trait]
impl StreamingCommand for DiceDumpCommand {
    const COMMAND_NAME: &'static str = "connected";

    async fn server_connect_options<'a, 'b>(
        &self,
        _ctx: &'b CommandContext,
    ) -> anyhow::Result<BuckdConnectOptions> {
        Ok(BuckdConnectOptions::existing_only())
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        _ctx: CommandContext,
    ) -> ExitResult {
        let format = if self.serde {
            DiceDumpFormat::Serde
        } else {
            DiceDumpFormat::Tsv
        };
        buckd
            .with_flushing(|client| {
                client
                    .unstable_dice_dump(UnstableDiceDumpRequest {
                        destination_path: self.path,
                        format: format.into(),
                    })
                    .boxed()
            })
            .await??;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        static OPTS: CommonConsoleOptions = CommonConsoleOptions {
            console_type: ConsoleType::None,
            ui: vec![],
        };
        &OPTS
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        CommonEventLogOptions::default_ref()
    }
}
