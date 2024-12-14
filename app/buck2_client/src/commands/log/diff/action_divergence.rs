/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_data::ActionKey;
use buck2_data::ActionName;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_observer::action_util::get_action_digest;
use buck2_event_observer::display::display_action_identity;
use buck2_event_observer::display::TargetDisplayOptions;
use futures::Stream;
use futures::TryStreamExt;
use linked_hash_map::LinkedHashMap;

use crate::commands::log::diff::diff_options::DiffEventLogOptions;

/// Identifies the first divergent action between two builds.
/// Divergence is identified by the same action having differing outputs. Useful for identifying non-determinism.
#[derive(Debug, clap::Parser)]
pub struct ActionDivergenceCommand {
    #[clap(flatten)]
    diff_event_log: DiffEventLogOptions,
}

#[derive(Clone, Debug)]
struct ActionExecutionData {
    name: Option<ActionName>,
    action_digest: Option<String>,
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
                                action_digest: get_action_digest(&data.commands),
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
    mut events: impl Stream<Item = buck2_error::Result<StreamValue>> + Unpin + Send,
) -> buck2_error::Result<LinkedHashMap<ActionKey, ActionExecutionData>> {
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
) -> buck2_error::Result<()> {
    let action_identity = display_action_identity(
        Some(action),
        ad2.name.as_ref(),
        TargetDisplayOptions::for_log(),
    )?;
    let header = match ad1 {
        Some(_) => "Present in both builds with differing output digests",
        None => "Present in only the second build",
    };
    let output = [
        format!("{:-^44}", "First Divergent Action"),
        header.to_owned(),
        action_identity,
        format!("{:-^44}", "Input Digest"),
        format!(
            "first: {} \t second: {}",
            ad1.and_then(|data| data.action_digest.as_deref())
                .unwrap_or("<none>"),
            ad2.action_digest.as_deref().unwrap_or("<none>"),
        ),
        format!("{:-^44}", "Tiny Output Digest(s)"),
        format!(
            "first: {} \t second: {}",
            ad1.map(|data| data.output_tiny_digests.as_ref())
                .unwrap_or("<none>"),
            ad2.output_tiny_digests
        ),
    ];
    buck2_client_ctx::println!("{}", output.join("\n"))?;

    Ok(())
}

impl ActionDivergenceCommand {
    pub fn exec(self, _matches: BuckArgMatches<'_>, ctx: ClientCommandContext<'_>) -> ExitResult {
        ctx.instant_command_no_log("log-diff-action-divergence", |ctx| async move {
            let (log_path1, log_path2) = self.diff_event_log.get(&ctx).await?;

            let (invocation1, events1) = log_path1.unpack_stream().await?;
            let (invocation2, events2) = log_path2.unpack_stream().await?;

            buck2_client_ctx::println!(
                "Analyzing divergent actions between: \n{} and \n{}",
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
            buck2_error::Ok(())
        })
        .into()
    }
}
