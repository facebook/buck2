/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use tokio_stream::StreamExt;

use crate::commands::log::options::EventLogOptions;
use crate::commands::log::transform_format;
use crate::commands::log::LogCommandOutputFormat;
use crate::commands::log::LogCommandOutputFormatWithWriter;

/// Outputs stats about uploads to RE from the selected invocation.
#[derive(Debug, clap::Parser)]
pub struct WhatUploadedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
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
    action: String,
    digests_uploaded: u64,
    bytes_uploaded: u64,
}

impl Display for Record {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\t{}\t{}",
            self.action, self.digests_uploaded, self.bytes_uploaded
        )
    }
}

fn get_record(
    state: &HashMap<u64, buck2_data::ActionExecutionStart>,
    upload: &ReUploadEvent,
) -> Record {
    let digests_uploaded = upload.inner.digests_uploaded.unwrap_or_default();
    let bytes_uploaded = upload.inner.bytes_uploaded.unwrap_or_default();
    let unknown = "unknown action".to_owned();
    let action_str = if let Some(action) = state.get(&upload.parent_span_id) {
        display::display_action_identity(
            action.key.as_ref(),
            action.name.as_ref(),
            TargetDisplayOptions::for_log(),
        )
        .unwrap_or(unknown)
    } else {
        unknown
    };
    Record {
        action: action_str,
        digests_uploaded,
        bytes_uploaded,
    }
}

fn print_uploads(
    output: &mut LogCommandOutputFormatWithWriter,
    record: &Record,
) -> anyhow::Result<()> {
    match output {
        LogCommandOutputFormatWithWriter::Tabulated(w) => {
            Ok(w.write_all(format!("{}\n", record).as_bytes())?)
        }
        LogCommandOutputFormatWithWriter::Csv(writer) => Ok(writer.serialize(record)?),
        LogCommandOutputFormatWithWriter::Json(w) => {
            serde_json::to_writer(w, &record)?;
            buck2_client_ctx::println!("")
        }
    }
}

struct ReUploadEvent<'a> {
    pub parent_span_id: u64,
    pub inner: &'a buck2_data::ReUploadEnd,
}

impl WhatUploadedCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        let Self { event_log, output } = self;

        buck2_client_ctx::stdio::print_with_writer::<anyhow::Error, _>(|w| {
            {
                let mut output = transform_format(output, w);
                ctx.with_runtime(async move |ctx| {
                    let log_path = event_log.get(&ctx).await?;

                    let (invocation, mut events) = log_path.unpack_stream().await?;
                    buck2_client_ctx::eprintln!(
                        "Showing uploads from: {}",
                        invocation.display_command_line()
                    )?;

                    let mut total_digests_uploaded = 0;
                    let mut total_bytes_uploaded = 0;
                    let mut state = HashMap::new();
                    while let Some(event) = events.try_next().await? {
                        match event {
                            // Insert parent span information so we can refer back to it later.
                            StreamValue::Event(event) => match event.data {
                                Some(buck2_data::buck_event::Data::SpanStart(start)) => match start
                                    .data
                                {
                                    Some(buck2_data::span_start_event::Data::ActionExecution(
                                        action,
                                    )) => {
                                        state.insert(event.span_id, action);
                                    }
                                    _ => {}
                                },

                                Some(buck2_data::buck_event::Data::SpanEnd(end)) => {
                                    match end.data.as_ref() {
                                        Some(buck2_data::span_end_event::Data::ReUpload(ref u)) => {
                                            let upload = ReUploadEvent {
                                                parent_span_id: event.parent_id,
                                                inner: u,
                                            };
                                            let record = get_record(&state, &upload);
                                            total_digests_uploaded += record.digests_uploaded;
                                            total_bytes_uploaded += record.bytes_uploaded;
                                            print_uploads(&mut output, &record)?;
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            },
                            StreamValue::Result(..) | StreamValue::PartialResult(..) => {}
                        }
                    }
                    buck2_client_ctx::eprintln!(
                        "total: digests: {}, bytes: {}",
                        total_digests_uploaded,
                        total_bytes_uploaded
                    )?;

                    anyhow::Ok(())
                })?;
                anyhow::Ok(())
            }
        })?;
        ExitResult::success()
    }
}
