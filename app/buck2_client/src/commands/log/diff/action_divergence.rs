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
use buck2_client_ctx::path_arg::PathArg;
use buck2_data::ActionKey;
use buck2_data::ActionName;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::display::display_action_identity;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_wrapper_common::invocation_id::TraceId;
use futures::Stream;
use futures::TryStreamExt;
use linked_hash_map::LinkedHashMap;

use crate::commands::log::options::EventLogOptions;

/// Identifies the first divergent action between two builds.
/// Divergence is identified by the same action having differing outputs. Useful for identifying non-determinism.
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::new("first").required(true))]
#[clap(group = clap::ArgGroup::new("second").required(true))]
pub struct ActionDivergenceCommand {
    /// A path to an event-log file of the first build.
    #[clap(long = "path1", group = "first")]
    path1: Option<PathArg>,
    /// Trace id of the first build.
    #[clap(long = "trace-id1", group = "first")]
    trace_id1: Option<TraceId>,
    /// Open the event-log file from a recent command for the first build.
    #[clap(long, group = "first", value_name = "NUMBER")]
    recent1: Option<usize>,
    /// A path to an event-log file of the second build.
    #[clap(long = "path2", group = "second")]
    path2: Option<PathArg>,
    /// Trace id of the second build.
    #[clap(long = "trace-id2", group = "second")]
    trace_id2: Option<TraceId>,
    /// Open the event-log file from a recent command for the second build.
    #[clap(long, group = "second", value_name = "NUMBER")]
    recent2: Option<usize>,
}

#[derive(Clone)]
struct ActionExecutionData {
    name: Option<ActionName>,
    output_tiny_digests: String,
}

fn get_action_execution_data<'a>(
    event: &'a buck2_data::BuckEvent,
) -> Option<(ActionKey, ActionExecutionData)> {
    event.data.as_ref().and_then(|data| match data {
        buck2_data::buck_event::Data::SpanEnd(end) => {
            end.data.as_ref().and_then(|data| match data {
                buck2_data::span_end_event::Data::ActionExecution(ref data) => {
                    data.key.as_ref().map(|key: &ActionKey| {
                        (
                            key.clone(),
                            ActionExecutionData {
                                name: data.name.clone(),
                                output_tiny_digests: data
                                    .outputs
                                    .iter()
                                    .fold(String::new(), |acc, action_output| {
                                        acc + " " + &action_output.tiny_digest
                                    }),
                            },
                        )
                    })
                }
                _ => None,
            })
        }
        _ => None,
    })
}

async fn get_digest_map(
    mut events: impl Stream<Item = anyhow::Result<StreamValue>> + Unpin + Send,
) -> anyhow::Result<LinkedHashMap<ActionKey, ActionExecutionData>> {
    let mut out = LinkedHashMap::new();

    while let Some(event) = events.try_next().await? {
        match event {
            StreamValue::Event(event) => match get_action_execution_data(&event) {
                Some((key, action_execution_data)) => {
                    out.insert(key, action_execution_data);
                }
                None => {
                    continue;
                }
            },
            _ => {}
        }
    }
    Ok(out)
}

fn print_divergence_msg(
    action: &ActionKey,
    ad1: Option<&ActionExecutionData>,
    ad2: &ActionExecutionData,
) -> anyhow::Result<()> {
    let action_identity = display_action_identity(
        Some(action),
        ad2.name.as_ref(),
        TargetDisplayOptions::for_log(),
    )?;
    match ad1 {
        Some(ad1) => buck2_client_ctx::println!(
            "This is the first action present in both builds with differing output digests: {}\ndigest(s)1 :  {} | digest(s)2: {}",
            action_identity,
            ad1.output_tiny_digests,
            ad2.output_tiny_digests
        )?,
        None => buck2_client_ctx::println!(
            "This is the first action present in only the second build: {}\ndigest(s)1 :  None | digest(s)2: {}",
            action_identity,
            ad2.output_tiny_digests
        )?,
    }
    Ok(())
}

impl ActionDivergenceCommand {
    pub fn exec(self, _matches: &clap::ArgMatches, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.with_runtime(|ctx| async move {
            let options1 = EventLogOptions {
                recent: self.recent1,
                path: self.path1,
                trace_id: self.trace_id1,
                no_remote: false,
                allow_remote: true,
            };
            let options2 = EventLogOptions {
                recent: self.recent2,
                path: self.path2,
                trace_id: self.trace_id2,
                no_remote: false,
                allow_remote: true,
            };

            let log_path1 = EventLogOptions::get(&options1, &ctx).await?;
            let log_path2 = EventLogOptions::get(&options2, &ctx).await?;

            let (invocation1, events1) = log_path1.unpack_stream().await?;
            let (invocation2, events2) = log_path2.unpack_stream().await?;

            buck2_client_ctx::println!(
                "Analzying divergent actions between: \n{} and \n{}",
                invocation1.display_command_line(),
                invocation2.display_command_line()
            )?;

            let digest_map1 = get_digest_map(events1).await?;
            let digest_map2 = get_digest_map(events2).await?;

            let mut divergence_found = false;

            for (action2, ad2) in digest_map2 {
                if let Some(ad1) = digest_map1.get(&action2).cloned() {
                    if ad1.output_tiny_digests == ad2.output_tiny_digests {
                        continue;
                    }
                    divergence_found = true;
                    print_divergence_msg(&action2, Some(&ad1), &ad2)?;
                } else {
                    divergence_found = true;
                    print_divergence_msg(&action2, None, &ad2)?;
                }
                break;
            }
            if !divergence_found {
                buck2_client_ctx::println!("No divergent actions found.")?;
            }
            anyhow::Ok(())
        })?;

        ExitResult::success()
    }
}
