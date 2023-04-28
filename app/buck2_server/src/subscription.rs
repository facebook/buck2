/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use anyhow::Context as _;
use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;
use futures::future::FutureExt;
use gazebo::prelude::*;
use tokio::time::MissedTickBehavior;

use crate::active_commands;
use crate::streaming_request_handler::StreamingRequestHandler;

pub(crate) async fn run_subscription_server_command(
    ctx: &dyn ServerCommandContextTrait,
    mut partial_result_dispatcher: PartialResultDispatcher<
        buck2_cli_proto::SubscriptionResponseWrapper,
    >,
    mut req: StreamingRequestHandler<buck2_cli_proto::SubscriptionRequestWrapper>,
) -> anyhow::Result<buck2_cli_proto::SubscriptionCommandResponse> {
    let metadata = ctx.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::SubscriptionCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result: anyhow::Result<buck2_cli_proto::SubscriptionCommandResponse> = try {
            // NOTE: Long term if we expose more things here then we should probably move this error to
            // only occur when we try to actually interact with materializer subscriptioons
            let materializer = ctx
                .materializer();

            let materializer = materializer
                .as_deferred_materializer_extension()
                .context("Subscriptions only work with the deferred materializer")?;

            let mut materializer_subscription = materializer
                .create_subscription()
                .await
                .context("Error creating a materializer subscription")?;

            let mut wants_active_commands = false;

            let mut ticker = tokio::time::interval(Duration::from_secs(1));
            ticker.set_missed_tick_behavior(MissedTickBehavior::Skip);

            loop {
                futures::select! {
                    message = req.message().fuse() => {
                        use buck2_subscription_proto::subscription_request::Request;

                        match message?.request.context("Empty message")?.request.context("Empty request")? {
                            Request::Disconnect(buck2_subscription_proto::Disconnect {}) => {
                                break;
                            }
                            Request::SubscribeToPaths(buck2_subscription_proto::SubscribeToPaths { paths }) => {
                                let paths = paths.into_try_map(|path| path.try_into())?;
                                materializer_subscription.subscribe_to_paths(paths);
                            }
                            Request::UnsubscribeFromPaths(buck2_subscription_proto::UnsubscribeFromPaths { paths }) => {
                                let paths = paths.into_try_map(|path| path.try_into())?;
                                materializer_subscription.unsubscribe_from_paths(paths);
                            }
                            Request::SubscribeToActiveCommands(buck2_subscription_proto::SubscribeToActiveCommands {}) => {
                                wants_active_commands = true;
                            }
                        }
                    }
                    path = materializer_subscription.next_materialization().fuse() => {
                        let path = path.context("Materializer hung up")?;
                        partial_result_dispatcher.emit(buck2_cli_proto::SubscriptionResponseWrapper {
                            response: Some(buck2_subscription_proto::SubscriptionResponse {
                                response: Some(buck2_subscription_proto::Materialized { path: path.to_string() }.into())
                            })
                        });
                    }
                    _ = ticker.tick().fuse() => {
                        if wants_active_commands {
                            let snapshot = active_commands_snapshot();
                            partial_result_dispatcher.emit(buck2_cli_proto::SubscriptionResponseWrapper {
                                response: Some(buck2_subscription_proto::SubscriptionResponse {
                                    response: Some(snapshot.into())
                                })
                            });
                        }
                    }
                }
            }

            buck2_cli_proto::SubscriptionCommandResponse {}
        };

        let end_event = command_end(metadata, &result, buck2_data::SubscriptionCommandEnd {});
        (result, end_event)
    })
    .await
}

fn active_commands_snapshot() -> buck2_subscription_proto::ActiveCommandsSnapshot {
    let active_commands = active_commands::active_commands()
        .iter()
        .map(|(trace_id, handle)| {
            let state = handle.state();
            let spans = state.spans();

            buck2_subscription_proto::ActiveCommand {
                trace_id: trace_id.to_string(),
                argv: state.argv.clone(),
                stats: Some(buck2_subscription_proto::ActiveCommandStats {
                    open_spans: spans.open as _,
                    closed_spans: spans.closed as _,
                }),
            }
        })
        .collect();

    buck2_subscription_proto::ActiveCommandsSnapshot { active_commands }
}
