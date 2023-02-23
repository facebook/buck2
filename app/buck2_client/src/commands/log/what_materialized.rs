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
use tokio::runtime;
use tokio_stream::StreamExt;

/// This command outputs materializations from the selected build. The output is a tab-separated
/// list containing the path, the materialization method, the file count, and the total size (after
/// decompression).
#[derive(Debug, clap::Parser)]
pub struct WhatMaterializedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

impl WhatMaterializedCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log } = self;

        let log_path = event_log.get(&ctx)?;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;

            buck2_client_ctx::eprintln!(
                "Showing materializations from: {}",
                shlex::join(invocation.command_line_args.iter().map(|e| e.as_str()))
            )?;

            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => {
                        match &event.data {
                            Some(buck2_data::buck_event::Data::SpanEnd(ref end)) => match end
                                .data
                                .as_ref()
                            {
                                Some(buck2_data::span_end_event::Data::Materialization(m)) => {
                                    // Only log what has been materialized.
                                    if !m.success {
                                        continue;
                                    }

                                    let method = match m
                                        .method
                                        .and_then(buck2_data::MaterializationMethod::from_i32)
                                    {
                                        Some(buck2_data::MaterializationMethod::CasDownload) => {
                                            "cas"
                                        }
                                        Some(buck2_data::MaterializationMethod::LocalCopy) => {
                                            "copy"
                                        }
                                        Some(buck2_data::MaterializationMethod::HttpDownload) => {
                                            "http"
                                        }
                                        Some(buck2_data::MaterializationMethod::Write) => "write",
                                        _ => "<unknown>",
                                    };

                                    buck2_client_ctx::println!(
                                        "{}\t{}\t{}\t{}",
                                        m.path,
                                        method,
                                        m.file_count,
                                        m.total_bytes
                                    )?;
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                    StreamValue::Result(..) => {}
                }
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
