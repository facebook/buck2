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
use buck2_client_ctx::tokio_runtime_setup::client_tokio_runtime;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;
use crate::commands::log::LogCommandOutputFormat;

/// This command outputs materializations from the selected build. The output is a tab-separated
/// list containing the path, the materialization method, the file count, and the total size (after
/// decompression).
#[derive(Debug, clap::Parser)]
pub struct WhatMaterializedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
    #[clap(
        long = "--format",
        help = "Which output format to use for this command",
        default_value = "tabulated",
        ignore_case = true,
        arg_enum
    )]
    pub output: LogCommandOutputFormat,
}

fn write_output(
    output: &LogCommandOutputFormat,
    materialization: &buck2_data::MaterializationEnd,
    method: &str,
) -> anyhow::Result<()> {
    #[derive(serde::Serialize)]
    struct Record<'a> {
        path: &'a str,
        method: &'a str,
        file_count: u64,
        total_bytes: u64,
    }

    match output {
        LogCommandOutputFormat::Tabulated => {
            buck2_client_ctx::println!(
                "{}\t{}\t{}\t{}",
                materialization.path,
                method,
                materialization.file_count,
                materialization.total_bytes
            )
        }
        LogCommandOutputFormat::Csv => buck2_client_ctx::stdio::print_with_writer(|w| {
            let mut writer = csv::WriterBuilder::new().has_headers(false).from_writer(w);
            writer.serialize(Record {
                path: &materialization.path,
                method,
                file_count: materialization.file_count,
                total_bytes: materialization.total_bytes,
            })
        }),
        LogCommandOutputFormat::Json => buck2_client_ctx::stdio::print_with_writer(|mut w| {
            let record = Record {
                path: &materialization.path,
                method,
                file_count: materialization.file_count,
                total_bytes: materialization.total_bytes,
            };
            serde_json::to_writer(&mut w, &record)?;
            w.write(b"\n").map(|_| ())
        }),
    }
}

impl WhatMaterializedCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log, output } = self;

        let rt = client_tokio_runtime()?;

        rt.block_on(async move {
            let log_path = event_log.get(&ctx).await?;

            let (invocation, mut events) = log_path.unpack_stream().await?;

            buck2_client_ctx::eprintln!("Showing materializations from: {}", invocation,)?;

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

                                    write_output(&output, m, method)?;
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                    StreamValue::Result(..) | StreamValue::PartialResult(..) => {}
                }
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
