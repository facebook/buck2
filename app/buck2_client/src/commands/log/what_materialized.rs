/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fmt::Formatter;
use std::path::Path;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use serde::Serialize;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;
use crate::commands::log::LogCommandOutputFormat;

/// Outputs materializations from selected invocation.
///
/// The output is a tab-separated list containing the path,
/// the materialization method, the file count, and the total size (after decompression).
#[derive(Debug, clap::Parser)]
pub struct WhatMaterializedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,

    #[clap(
        long = "sort-by-size",
        short = 's',
        help = "Sort the output by total bytes in ascending order",
        conflicts_with = "aggregate-by-ext"
    )]
    sort_by_total_bytes: bool,

    /// Aggregates the output by file extension
    #[clap(long, conflicts_with = "sort-by-total-bytes")]
    aggregate_by_ext: bool,

    #[clap(
        long = "format",
        help = "Which output format to use for this command",
        default_value = "tabulated",
        ignore_case = true,
        arg_enum
    )]
    pub output: LogCommandOutputFormat,
}

#[derive(serde::Serialize)]
struct Record {
    path: String,
    method: &'static str,
    file_count: u64,
    total_bytes: u64,
}

impl Display for Record {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}",
            self.path, self.method, self.file_count, self.total_bytes
        )
    }
}

#[derive(Eq, Ord, PartialEq, PartialOrd)]
struct AggregationKey<'a> {
    extension: &'a str,
    method: &'static str,
}

#[derive(serde::Serialize)]
struct AggregatedRecord<'a> {
    extension: &'a str,
    method: &'static str,
    file_count: u64,
    total_bytes: u64,
}

impl<'a> AggregatedRecord<'a> {
    fn update(&mut self, value: &Record) {
        self.file_count += value.file_count;
        self.total_bytes += value.total_bytes;
    }

    fn get_key(&self) -> AggregationKey<'a> {
        AggregationKey {
            extension: self.extension,
            method: self.method,
        }
    }
}

impl<'a> Display for AggregatedRecord<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}",
            self.extension, self.method, self.file_count, self.total_bytes
        )
    }
}

impl<'a> From<&'a Record> for AggregatedRecord<'a> {
    fn from(value: &'a Record) -> Self {
        Self {
            extension: Path::new(&value.path)
                .extension()
                .and_then(OsStr::to_str)
                .unwrap_or("<empty>"),
            method: value.method,
            file_count: value.file_count,
            total_bytes: value.total_bytes,
        }
    }
}

fn write_output<T: Display + Serialize>(
    output: &LogCommandOutputFormat,
    record: &T,
) -> anyhow::Result<()> {
    match output {
        LogCommandOutputFormat::Tabulated => {
            buck2_client_ctx::println!("{}", record)
        }
        LogCommandOutputFormat::Csv => buck2_client_ctx::stdio::print_with_writer(|w| {
            let mut writer = csv::WriterBuilder::new().has_headers(false).from_writer(w);
            writer.serialize(record)
        }),
        LogCommandOutputFormat::Json => buck2_client_ctx::stdio::print_with_writer(|mut w| {
            serde_json::to_writer(&mut w, &record)?;
            w.write(b"\n").map(|_| ())
        }),
    }
}

fn get_record(materialization: &buck2_data::MaterializationEnd) -> Record {
    let method = match materialization
        .method
        .and_then(buck2_data::MaterializationMethod::from_i32)
    {
        Some(buck2_data::MaterializationMethod::CasDownload) => "cas",
        Some(buck2_data::MaterializationMethod::LocalCopy) => "copy",
        Some(buck2_data::MaterializationMethod::HttpDownload) => "http",
        Some(buck2_data::MaterializationMethod::Write) => "write",
        _ => "<unknown>",
    };
    Record {
        path: materialization.path.clone(),
        method,
        file_count: materialization.file_count,
        total_bytes: materialization.total_bytes,
    }
}

impl WhatMaterializedCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self {
            event_log,
            output,
            sort_by_total_bytes,
            aggregate_by_ext,
        } = self;

        ctx.with_runtime(async move |ctx| {
            let log_path = event_log.get(&ctx).await?;

            let (invocation, mut events) = log_path.unpack_stream().await?;

            buck2_client_ctx::eprintln!(
                "Showing materializations from: {}",
                invocation.display_command_line()
            )?;

            let mut records: Vec<Record> = Vec::new();
            while let Some(event) = events.try_next().await? {
                match event {
                    StreamValue::Event(event) => match &event.data {
                        Some(buck2_data::buck_event::Data::SpanEnd(buck2_data::SpanEndEvent {
                            data: Some(buck2_data::span_end_event::Data::Materialization(m)),
                            ..
                        })) if m.success =>
                        // Only log what has been materialized.
                        {
                            if sort_by_total_bytes || aggregate_by_ext {
                                records.push(get_record(m));
                            } else {
                                write_output(&output, &get_record(m))?
                            }
                        }
                        _ => {}
                    },
                    StreamValue::Result(..) | StreamValue::PartialResult(..) => {}
                };
            }

            if aggregate_by_ext {
                let mut kv: BTreeMap<AggregationKey, AggregatedRecord> = BTreeMap::new();
                for r in records.iter() {
                    let v: AggregatedRecord = r.into();
                    let k = v.get_key();
                    kv.entry(k).and_modify(|e| e.update(r)).or_insert(v);
                }
                kv.iter().try_for_each(|(_, v)| write_output(&output, v))?;
            } else if sort_by_total_bytes {
                records.sort_by(|a, b| a.total_bytes.cmp(&b.total_bytes));
                records.iter().try_for_each(|r| write_output(&output, r))?;
            }

            anyhow::Ok(())
        })?;
        ExitResult::success()
    }
}
