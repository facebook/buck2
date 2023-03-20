/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_cli_proto::CommandResult;
use buck2_data::buck_event;
use buck2_data::InstantEvent;
use buck2_data::SpanEndEvent;
use buck2_data::SpanStartEvent;
use buck2_event_observer::unpack_event::VisitorError;
use buck2_events::BuckEvent;

use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber::Tick;

/// Wrap an `UnpackingEventSubscriber` instance to provide an `EventSubscriber`.
pub(crate) struct UnpackingEventSubscriberAsEventSubscriber<U: UnpackingEventSubscriber>(
    pub(crate) U,
);

#[async_trait]
pub trait UnpackingEventSubscriber: Send {
    async fn handle_output(&mut self, _raw_output: &[u8]) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_stderr(&mut self, _stderr: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_console_interaction(&mut self, _c: char) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        self.handle_inner_event(event).await
    }
    async fn handle_inner_event(&mut self, event: &Arc<BuckEvent>) -> anyhow::Result<()> {
        match event.data() {
            buck_event::Data::SpanStart(ref start) => self.handle_event_start(start, event),
            buck_event::Data::SpanEnd(ref end) => self.handle_event_end(end, event),
            buck_event::Data::Instant(ref instant) => self.handle_instant(instant, event),
            // Not present in the event stream from the daemon to CLI.
            buck_event::Data::Record(_) => Box::pin(async { Ok(()) }),
        }
        .await
    }
    async fn handle_command_result(&mut self, _result: &CommandResult) -> anyhow::Result<()> {
        Ok(())
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        Ok(())
    }

    async fn handle_event_start(
        &mut self,
        start: &SpanStartEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match start
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::span_start_event::Data::Command(command) => {
                self.handle_command_start(command, event).await
            }
            _ => Ok(()),
        }
    }

    async fn handle_event_end(
        &mut self,
        end: &SpanEndEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match end
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::span_end_event::Data::Command(command) => {
                self.handle_command_end(command, event).await
            }
            buck2_data::span_end_event::Data::ActionExecution(action) => {
                self.handle_action_execution_end(action, event).await
            }
            buck2_data::span_end_event::Data::FileWatcher(file_watcher) => {
                self.handle_file_watcher_end(file_watcher, event).await
            }
            _ => Ok(()),
        }
    }

    async fn handle_instant(
        &mut self,
        instant: &InstantEvent,
        event: &Arc<BuckEvent>,
    ) -> anyhow::Result<()> {
        match instant
            .data
            .as_ref()
            .ok_or_else(|| VisitorError::MissingField((**event).clone()))?
        {
            buck2_data::instant_event::Data::ConsoleMessage(message) => {
                self.handle_console_message(message, event).await
            }
            buck2_data::instant_event::Data::ReSession(session) => {
                self.handle_re_session_created(session, event).await
            }
            buck2_data::instant_event::Data::StructuredError(err) => {
                self.handle_structured_error(err, event).await
            }
            buck2_data::instant_event::Data::TestDiscovery(discovery) => {
                self.handle_test_discovery(discovery, event).await
            }
            buck2_data::instant_event::Data::TestResult(result) => {
                self.handle_test_result(result, event).await
            }
            buck2_data::instant_event::Data::ConsolePreferences(preferences) => {
                self.handle_console_preferences(preferences, event).await
            }
            _ => Ok(()),
        }
    }

    async fn handle_command_start(
        &mut self,
        _command: &buck2_data::CommandStart,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_command_end(
        &mut self,
        _command: &buck2_data::CommandEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_action_execution_end(
        &mut self,
        _action: &buck2_data::ActionExecutionEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_file_watcher_end(
        &mut self,
        _watchman: &buck2_data::FileWatcherEnd,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_console_message(
        &mut self,
        _message: &buck2_data::ConsoleMessage,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_re_session_created(
        &mut self,
        _session: &buck2_data::RemoteExecutionSessionCreated,
        _event: &BuckEvent,
    ) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_structured_error(
        &mut self,
        _err: &buck2_data::StructuredError,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_test_discovery(
        &mut self,
        _test_info: &buck2_data::TestDiscovery,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_test_result(
        &mut self,
        _result: &buck2_data::TestResult,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    async fn handle_console_preferences(
        &mut self,
        _prefs: &buck2_data::ConsolePreferences,
        _event: &BuckEvent,
    ) -> anyhow::Result<()>;

    /// Give the subscriber a chance to react to errors as we start trying to clean up.
    /// They may return another error, which will be incorporated into the end result.
    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()>;

    /// Allow the subscriber to do some sort of action once every render cycle.
    async fn tick(&mut self, _tick: &Tick) -> anyhow::Result<()>;
}

#[async_trait]
impl<U: UnpackingEventSubscriber> EventSubscriber for UnpackingEventSubscriberAsEventSubscriber<U> {
    async fn handle_output(&mut self, raw_output: &[u8]) -> anyhow::Result<()> {
        self.0.handle_output(raw_output).await
    }

    async fn handle_tailer_stderr(&mut self, stderr: &str) -> anyhow::Result<()> {
        self.0.handle_stderr(stderr).await
    }

    async fn handle_console_interaction(&mut self, c: char) -> anyhow::Result<()> {
        self.0.handle_console_interaction(c).await
    }

    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        for event in events {
            self.0.handle_event(event).await?;
        }
        Ok(())
    }

    async fn handle_error(&mut self, error: &anyhow::Error) -> anyhow::Result<()> {
        self.0.handle_error(error).await
    }

    async fn tick(&mut self, tick: &Tick) -> anyhow::Result<()> {
        self.0.tick(tick).await
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        self.0.handle_command_result(result).await
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.0.exit().await
    }
}
