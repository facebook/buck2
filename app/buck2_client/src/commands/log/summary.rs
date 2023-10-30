/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_data::ActionExecutionKind;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;

#[derive(Default)]
struct Stats {
    // TODO(yurysamkevich): add number of file changes since last build once availbale in log
    total_bytes_uploaded: u64,
    total_files_materialized: u64,
    total_bytes_materialized: u64,
    total_local_actions: u64,
    // TODO(yurysamkevich): split by RE platform - mac/windows/linux once available in log
    total_remote_actions: u64,
    total_other_actions: u64,
    total_targets_analysed: u64,
}

impl Stats {
    fn update_with_event(&mut self, event: &buck2_data::BuckEvent) {
        match &event.data {
            Some(buck2_data::buck_event::Data::SpanEnd(end)) => match end.data.as_ref() {
                Some(buck2_data::span_end_event::Data::ReUpload(ref data)) => {
                    self.total_bytes_uploaded += data.bytes_uploaded.unwrap_or_default();
                }
                Some(buck2_data::span_end_event::Data::Materialization(ref data)) => {
                    self.total_files_materialized += data.file_count;
                    self.total_bytes_materialized += data.total_bytes;
                }
                Some(buck2_data::span_end_event::Data::ActionExecution(ref data)) => {
                    match ActionExecutionKind::from_i32(data.execution_kind) {
                        Some(ActionExecutionKind::Local) => self.total_local_actions += 1,
                        Some(ActionExecutionKind::Remote) => self.total_remote_actions += 1,
                        Some(ActionExecutionKind::ActionCache) => self.total_remote_actions += 1,
                        _ => self.total_other_actions += 1,
                    }
                }
                Some(buck2_data::span_end_event::Data::Analysis(_)) => {
                    self.total_targets_analysed += 1;
                }
                _ => {}
            },
            _ => {}
        }
    }
}

impl Display for Stats {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "total files materialized: {}",
            self.total_files_materialized
        )?;
        writeln!(
            f,
            "total bytes materialized: {}",
            self.total_bytes_materialized
        )?;
        writeln!(f, "total bytes uploaded: {}", self.total_bytes_uploaded)?;
        writeln!(f, "local actions: {}", self.total_local_actions)?;
        writeln!(f, "remote actions: {}", self.total_remote_actions)?;
        writeln!(f, "other actions: {}", self.total_other_actions)?;
        writeln!(f, "targets analysed: {}", self.total_targets_analysed)
    }
}

/// Outputs high level statistics about the build
#[derive(Debug, clap::Parser)]
pub struct SummaryCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl SummaryCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.with_runtime(async move |ctx| {
            let log_path = self.event_log.get(&ctx).await?;

            let (invocation, mut events) = log_path.unpack_stream().await?;

            buck2_client_ctx::eprintln!(
                "Showing summary from: {}",
                invocation.display_command_line()
            )?;

            let mut stats = Stats::default();

            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => stats.update_with_event(&event),
                    StreamValue::Result(..) | StreamValue::PartialResult(..) => {}
                }
            }
            buck2_client_ctx::eprintln!("{}", stats)?;
            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}
