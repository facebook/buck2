/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::subscribers::event_log::read::ReaderStats;
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;

/// This command outputs the most recent log in JSON format
#[derive(Debug, clap::Parser)]
pub struct LogPerfCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    /// Stats will be emitted every `interval` events.
    #[clap(long, default_value = "10000")]
    interval: u64,
}

impl LogPerfCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log,
            interval,
        } = self;

        let rt = client_tokio_runtime()?;

        rt.block_on(async move {
            let log_path = event_log.get(&ctx).await?;

            let mut total_alloc = 0;

            let stats = ReaderStats::new();
            let (_invocation, mut events) = log_path.unpack_stream_with_stats(&stats).await?;

            let mut i = 0;

            while let Some(event) = events.try_next().await? {
                total_alloc += allocative::size_of_unique(&event);

                if i % interval == 0 {
                    buck2_client_ctx::println!(
                        "{}\t{}\t{}\t{}",
                        i,
                        stats.compressed_bytes(),
                        stats.decompressed_bytes(),
                        total_alloc
                    )?;
                }

                i += 1;
            }
            buck2_client_ctx::println!(
                "{}\t{}\t{}\t{}",
                i,
                stats.compressed_bytes(),
                stats.decompressed_bytes(),
                total_alloc
            )?;

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
