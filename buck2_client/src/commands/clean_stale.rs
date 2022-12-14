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
use buck2_client_ctx::streaming::StreamingCommand;
use chrono::DateTime;
use chrono::Duration;
use chrono::TimeZone;
use chrono::Utc;
use cli_proto::CleanStaleRequest;
use cli_proto::CleanStaleResponse;
use humantime;

/// Clean only old artifacts from a running buck daemon without killing the daemon.
///
/// This is a separate command from CleanCommand even though it is invoked with
/// a flag (--stale) on the clean subcommand, which is a bit weird.
/// This is just so that it can be used as a StreamingCommand, which CleanCommand should not be.
pub struct CleanStaleCommand {
    pub console_opts: CommonConsoleOptions,
    pub config_opts: CommonBuildConfigurationOptions,
    pub event_log_opts: CommonDaemonCommandOptions,
    pub keep_since_arg: KeepSinceArg,
    pub dry_run: bool,
}

/// Specifies the maximum age of artifacts to keep
pub enum KeepSinceArg {
    Duration(Duration),
    Time(i64),
}

pub fn parse_clean_stale_args(
    stale: Option<Option<humantime::Duration>>,
    keep_since_time: Option<i64>,
) -> anyhow::Result<Option<KeepSinceArg>> {
    let arg = match (stale, keep_since_time) {
        (Some(Some(human_duration)), None) => {
            let duration = chrono::Duration::from_std(human_duration.into())?;
            Some(KeepSinceArg::Duration(duration))
        }
        (Some(None), None) => Some(KeepSinceArg::Duration(chrono::Duration::weeks(1))),
        (None, Some(time)) => Some(KeepSinceArg::Time(time)),
        (Some(_), Some(_)) => unreachable!("keep-since-time conflicts_with stale"),
        (None, None) => None,
    };
    Ok(arg)
}

#[async_trait]
impl StreamingCommand for CleanStaleCommand {
    const COMMAND_NAME: &'static str = "clean";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let keep_since_time = match self.keep_since_arg {
            KeepSinceArg::Duration(duration) => {
                let keep_since_time: DateTime<Utc> = Utc::now()
                    .checked_sub_signed(duration)
                    .context("Duration underflow")?;

                buck2_client_ctx::eprintln!(
                    "Cleaning artifacts more than {} old",
                    humantime::format_duration(
                        duration.to_std().context("Error converting duration")?
                    ),
                )?;
                keep_since_time
            }
            KeepSinceArg::Time(timestamp) => Utc
                .timestamp_opt(timestamp, 0)
                .single()
                .context("Invalid timestamp")?,
        };

        let context = ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;
        let response: CleanStaleResponse = buckd
            .with_flushing()
            .clean_stale(
                CleanStaleRequest {
                    context: Some(context),
                    keep_since_time: keep_since_time.timestamp(),
                    dry_run: self.dry_run,
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
            )
            .await??;
        buck2_client_ctx::eprintln!("{}", response.response)?;
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
