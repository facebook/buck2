/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::stream_value::StreamValue;
use buck2_client_ctx::subscribers::event_log::options::EventLogOptions;
use buck2_event_observer::display;
use buck2_event_observer::display::TargetDisplayOptions;
use tokio::runtime;
use tokio_stream::StreamExt;

/// This command outputs stats about uploads to RE from the selected invocation.
#[derive(Debug, clap::Parser)]
pub struct WhatUploadedCommand {
    #[clap(flatten)]
    event_log: EventLogOptions,
}

fn print_uploads(
    upload: ReUploadEvent,
    state: &HashMap<u64, buck2_data::ActionExecutionStart>,
    total_digests: &mut u64,
    total_bytes: &mut u64,
) -> anyhow::Result<()> {
    let digests_uploaded = upload.inner.digests_uploaded.unwrap_or_default();
    let bytes_uploaded = upload.inner.bytes_uploaded.unwrap_or_default();
    *total_digests += digests_uploaded;
    *total_bytes += bytes_uploaded;

    let action_str = if let Some(action) = state.get(&upload.parent_span_id) {
        display::display_action_identity(
            action.key.as_ref(),
            action.name.as_ref(),
            TargetDisplayOptions::for_log(),
        )?
    } else {
        "unknown action".to_owned()
    };
    buck2_client_ctx::println!("{}\t{}\t{}", action_str, digests_uploaded, bytes_uploaded)?;

    Ok(())
}

struct ReUploadEvent<'a> {
    pub parent_span_id: u64,
    pub inner: &'a buck2_data::ReUploadEnd,
}

impl WhatUploadedCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext) -> ExitResult {
        let Self { event_log } = self;

        let log_path = event_log.get(&ctx)?;

        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        rt.block_on(async move {
            let (invocation, mut events) = log_path.unpack_stream().await?;
            buck2_client_ctx::eprintln!("Showing uploads from: {}", invocation,)?;

            let mut total_digests_uploaded = 0;
            let mut total_bytes_uploaded = 0;
            let mut state = HashMap::new();
            while let Some(event) = events.try_next().await? {
                match event {
                    // Insert parent span information so we can refer back to it later.
                    StreamValue::Event(event) => match event.data {
                        Some(buck2_data::buck_event::Data::SpanStart(start)) => match start.data {
                            Some(buck2_data::span_start_event::Data::ActionExecution(action)) => {
                                state.insert(event.span_id, action);
                            }
                            _ => {}
                        },
                        Some(buck2_data::buck_event::Data::SpanEnd(end)) => {
                            match end.data.as_ref() {
                                Some(buck2_data::span_end_event::Data::ReUpload(ref u)) => {
                                    print_uploads(
                                        ReUploadEvent {
                                            parent_span_id: event.parent_id,
                                            inner: u,
                                        },
                                        &state,
                                        &mut total_digests_uploaded,
                                        &mut total_bytes_uploaded,
                                    )?;
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

        ExitResult::success()
    }
}
