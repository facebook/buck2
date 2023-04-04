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
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::options::EventLogOptions;
use tokio_stream::StreamExt;

/// This command outputs stats about uploads to RE from the selected invocation.
#[derive(Debug, clap::Parser)]
pub struct CriticalPathCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl CriticalPathCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log } = self;

        let log_path = event_log.get(&ctx)?;

        let rt = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;
            buck2_client_ctx::eprintln!("Showing critical path from: {}", invocation)?;

            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => match event.data {
                        Some(buck2_data::buck_event::Data::Instant(instant)) => {
                            match instant.data {
                                Some(buck2_data::instant_event::Data::BuildGraphInfo(
                                    build_graph,
                                )) => {
                                    log_critical_path(&build_graph)?;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}

fn log_critical_path(_critical_path: &buck2_data::BuildGraphExecutionInfo) -> anyhow::Result<()> {
    Ok(())
}
