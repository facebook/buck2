/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use async_trait::async_trait;
use buck2_cli_proto::CleanStaleRequest;
use buck2_cli_proto::CleanStaleResponse;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::common::CommonBuildConfigurationOptions;
use buck2_client_ctx::common::CommonCommandOptions;
use buck2_client_ctx::common::CommonEventLogOptions;
use buck2_client_ctx::common::CommonStarlarkOptions;
use buck2_client_ctx::common::ui::CommonConsoleOptions;
use buck2_client_ctx::daemon::client::BuckdClientConnector;
use buck2_client_ctx::daemon::client::NoPartialResultHandler;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::streaming::StreamingCommand;
use buck2_error::BuckErrorContext;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use chrono::DateTime;
use chrono::Duration;
use chrono::TimeZone;
use chrono::Utc;

/// Clean only old artifacts from a running buck daemon without killing the daemon.
/// This can be interrupted by other commands that run in parallel and request materialization.
///
/// This is a separate command from CleanCommand even though it is invoked with
/// a flag (--stale) on the clean subcommand, which is a bit weird.
/// This is just so that it can be used as a StreamingCommand, which CleanCommand should not be.
pub struct CleanStaleCommand {
    pub(crate) common_opts: CommonCommandOptions,
    pub keep_since_arg: KeepSinceArg,
    pub dry_run: bool,
    pub tracked_only: bool,
}

/// Specifies the maximum age of artifacts to keep
pub enum KeepSinceArg {
    Duration(Duration),
    Time(i64),
}

pub fn parse_clean_stale_args(
    stale: Option<Option<humantime::Duration>>,
    keep_since_time: Option<i64>,
) -> buck2_error::Result<Option<KeepSinceArg>> {
    let arg = match (stale, keep_since_time) {
        (Some(Some(human_duration)), None) => {
            let duration = chrono::Duration::from_std(human_duration.into())
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::InvalidDuration))?;
            Some(KeepSinceArg::Duration(duration))
        }
        (Some(None), None) => Some(KeepSinceArg::Duration(chrono::Duration::weeks(1))),
        (None, Some(time)) => Some(KeepSinceArg::Time(time)),
        (Some(_), Some(_)) => unreachable!("keep-since-time conflicts_with stale"),
        (None, None) => None,
    };
    Ok(arg)
}

fn format_result_stats(stats: buck2_data::CleanStaleStats) -> String {
    let mut output = String::new();
    output += &format!(
        "Found {} stale artifacts ({})\n",
        stats.stale_artifact_count,
        bytesize::ByteSize::b(stats.stale_bytes).display().iec(),
    );
    output += &format!(
        "Found {} recent artifacts ({})\n",
        stats.retained_artifact_count,
        bytesize::ByteSize::b(stats.retained_bytes).display().iec(),
    );
    output += &format!(
        "Found {} untracked artifacts ({})\n",
        stats.untracked_artifact_count,
        bytesize::ByteSize::b(stats.untracked_bytes).display().iec(),
    );
    if stats.cleaned_artifact_count > 0 || stats.cleaned_bytes > 0 {
        output += &format!("Cleaned {} paths\n", stats.cleaned_artifact_count,);
        output += &format!(
            "{} bytes cleaned ({})\n",
            stats.cleaned_bytes,
            bytesize::ByteSize::b(stats.cleaned_bytes).display().iec(),
        );
    }
    output
}

#[async_trait(?Send)]
impl StreamingCommand for CleanStaleCommand {
    const COMMAND_NAME: &'static str = "clean-stale";

    async fn exec_impl(
        self,
        buckd: &mut BuckdClientConnector,
        matches: BuckArgMatches<'_>,
        ctx: &mut ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        let keep_since_time = match self.keep_since_arg {
            KeepSinceArg::Duration(duration) => {
                let keep_since_time: DateTime<Utc> = Utc::now()
                    .checked_sub_signed(duration)
                    .ok_or_else(|| internal_error!("Duration underflow"))?;
                buck2_client_ctx::eprintln!(
                    "Cleaning artifacts more than {} old",
                    humantime::format_duration(
                        duration
                            .to_std()
                            .map_err(|e| from_any_with_tag(
                                e,
                                buck2_error::ErrorTag::InvalidDuration
                            ))
                            .buck_error_context("Error converting duration")?
                    ),
                )?;
                // Round up to next second since timestamp below is rounded down
                // (this way clean --stale=0s immediately after a build deletes the result)
                keep_since_time + chrono::Duration::seconds(1)
            }
            KeepSinceArg::Time(timestamp) => Utc
                .timestamp_opt(timestamp, 0)
                .single()
                .ok_or_else(|| internal_error!("Invalid timestamp"))?,
        };

        let context = ctx.client_context(matches, &self)?;
        let response: CleanStaleResponse = buckd
            .with_flushing()
            .clean_stale(
                CleanStaleRequest {
                    context: Some(context),
                    keep_since_time: keep_since_time.timestamp(),
                    dry_run: self.dry_run,
                    tracked_only: self.tracked_only,
                },
                events_ctx,
                ctx.console_interaction_stream(&self.common_opts.console_opts),
                &mut NoPartialResultHandler,
            )
            .await??;

        if let Some(message) = response.message {
            buck2_client_ctx::eprintln!("{}", message)?;
        }
        if let Some(stats) = response.stats {
            buck2_client_ctx::eprintln!("{}", format_result_stats(stats))?;
        }
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
