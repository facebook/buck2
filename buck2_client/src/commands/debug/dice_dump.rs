/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use async_trait::async_trait;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonConsoleOptions;
use buck2_client_ctx::common::CommonDaemonCommandOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_client_ctx::streaming::StreamingCommand;
use cli_proto::unstable_dice_dump_request::DiceDumpFormat;
use cli_proto::UnstableDiceDumpRequest;

#[derive(Debug, clap::Parser)]
pub struct DiceDumpCommand {
    /// The path to write the heap dump to.
    #[clap(short, long, value_name = "PATH")]
    path: PathArg,
    #[clap(long, group = "dice_dump_format")]
    serde: bool,
    #[clap(long, group = "dice_dump_format")]
    serde_pretty: bool,
}

#[async_trait]
impl StreamingCommand for DiceDumpCommand {
    const COMMAND_NAME: &'static str = "connected";

    fn existing_only() -> bool {
        true
    }

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        _matches: &clap::ArgMatches,
        ctx: ClientCommandContext,
    ) -> ExitResult {
        let format = if self.serde {
            DiceDumpFormat::Bincode
        } else if self.serde_pretty {
            DiceDumpFormat::JsonPretty
        } else {
            DiceDumpFormat::Tsv
        };
        buckd
            .with_flushing()
            .unstable_dice_dump(UnstableDiceDumpRequest {
                destination_path: self
                    .path
                    .resolve(&ctx.working_dir)
                    .to_str()
                    .context("path is not UTF-8")?
                    .to_owned(),
                format: format.into(),
            })
            .await?;
        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        CommonConsoleOptions::none_ref()
    }

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        CommonDaemonCommandOptions::default_ref()
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        CommonBuildConfigurationOptions::default_ref()
    }
}
