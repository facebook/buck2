/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_events::dispatch::span_async;
use dice::DiceTransaction;

use crate::command_end::command_end_ext;
use crate::ctx::ServerCommandContextTrait;
use crate::ctx::ServerCommandDiceContext;
use crate::logging::TracingLogFile;
use crate::partial_result_dispatcher::PartialResultDispatcher;

/// Typical server command with DICE and span.
#[async_trait]
pub trait ServerCommandTemplate: Send + Sync {
    /// Event to send in the beginning of command.
    type StartEvent: Into<buck2_data::command_start::Data> + Default;
    /// Event to send in the end of command.
    type EndEvent: Into<buck2_data::command_end::Data> + Default;
    /// Command return type.
    /// TODO: This is called `Result` everywhere, we should probably be consistent.
    type Response;
    /// Command partial response.
    type PartialResult: Send + Sync;

    /// Create start event. Called before command is invoked.
    fn start_event(&self) -> Self::StartEvent {
        Self::StartEvent::default()
    }

    /// Create end event. Called after command is invoked.
    fn end_event(&self, _response: &anyhow::Result<Self::Response>) -> Self::EndEvent {
        Self::EndEvent::default()
    }

    /// Set `buck2_data::CommandEnd::is_success` to
    /// * `command` returns `Ok`
    /// * and this function returns `true`
    fn is_success(&self, response: &Self::Response) -> bool;

    /// If not `None`, command will block and be blocked by any concurrent commands.
    fn exclusive_command_name(&self) -> Option<String> {
        None
    }

    /// Command implementation.
    async fn command<'v>(
        &self,
        server_ctx: &'v dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<Self::PartialResult>,
        ctx: DiceTransaction,
    ) -> anyhow::Result<Self::Response>;
}

/// Call this function to run the command template implementation.
pub async fn run_server_command<T: ServerCommandTemplate>(
    command: T,
    server_ctx: Box<dyn ServerCommandContextTrait>,
    partial_result_dispatcher: PartialResultDispatcher<<T as ServerCommandTemplate>::PartialResult>,
) -> anyhow::Result<T::Response> {
    let metadata = server_ctx.request_metadata().await?;
    let start_event = buck2_data::CommandStart {
        metadata: metadata.clone(),
        data: Some(command.start_event().into()),
    };

    // refresh our tracing log per command
    TracingLogFile::refresh()?;

    span_async(start_event, async {
        let result = server_ctx
            .with_dice_ctx_maybe_exclusive(
                |server_ctx, ctx| command.command(server_ctx, partial_result_dispatcher, ctx),
                command.exclusive_command_name(),
            )
            .await;
        let end_event = command_end_ext(metadata, &result, command.end_event(&result), |result| {
            command.is_success(result)
        });
        (result, end_event)
    })
    .await
}
