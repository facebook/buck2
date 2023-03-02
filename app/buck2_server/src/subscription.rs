/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_events::dispatch::span_async;
use buck2_server_ctx::command_end::command_end;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use buck2_server_ctx::partial_result_dispatcher::PartialResultDispatcher;

use crate::streaming_request_handler::StreamingRequestHandler;

pub(crate) async fn run_subscription_server_command(
    ctx: Box<dyn ServerCommandContextTrait>,
    _partial_result_dispatcher: PartialResultDispatcher<
        buck2_cli_proto::SubscriptionResponseWrapper,
    >,
    _req: StreamingRequestHandler<buck2_cli_proto::SubscriptionRequestWrapper>,
) -> anyhow::Result<buck2_cli_proto::SubscriptionCommandResponse> {
    let metadata = ctx.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(buck2_data::SubscriptionCommandStart {}.into()),
    };
    span_async(start_event, async move {
        let result = Ok(buck2_cli_proto::SubscriptionCommandResponse {});
        // TODO: Fill in the implementation!
        let end_event = command_end(metadata, &result, buck2_data::SubscriptionCommandEnd {});
        (result, end_event)
    })
    .await
}
