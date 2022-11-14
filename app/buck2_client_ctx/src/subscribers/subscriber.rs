/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::time::Duration;
use std::time::Instant;

use async_trait::async_trait;
use buck2_data::buck_event;
use buck2_data::InstantEvent;
use buck2_data::SpanEndEvent;
use buck2_data::SpanStartEvent;
use buck2_events::BuckEvent;
use gazebo::prelude::*;
use thiserror::Error;

/// Information about tick timing.
#[derive(Debug, Clone, Dupe)]
pub struct Tick {
    /// The time that the ticker was started.
    pub(crate) start_time: Instant,
    /// Elapsed time since the ticker was started for this tick.
    pub(crate) elapsed_time: Duration,
}

impl Tick {
    pub(crate) fn now() -> Tick {
        Self {
            start_time: Instant::now(),
            elapsed_time: Duration::ZERO,
        }
    }
}

/// Just a simple structure that makes it easier to deal with BuckEvent rather than
/// needing to deal with the unpacking of optional fields yourself.
pub(crate) enum UnpackedBuckEvent<'a> {
    SpanStart(
        &'a BuckEvent,
        &'a SpanStartEvent,
        &'a buck2_data::span_start_event::Data,
    ),
    SpanEnd(
        &'a BuckEvent,
        &'a SpanEndEvent,
        &'a buck2_data::span_end_event::Data,
    ),
    Instant(
        &'a BuckEvent,
        &'a InstantEvent,
        &'a buck2_data::instant_event::Data,
    ),
}

pub(crate) fn unpack_event(event: &BuckEvent) -> anyhow::Result<UnpackedBuckEvent> {
    match &event.data() {
        buck_event::Data::SpanStart(v) => Ok(UnpackedBuckEvent::SpanStart(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField(event.clone()))?,
        )),
        buck_event::Data::SpanEnd(v) => Ok(UnpackedBuckEvent::SpanEnd(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField(event.clone()))?,
        )),
        buck_event::Data::Instant(v) => Ok(UnpackedBuckEvent::Instant(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?,
        )),
        buck_event::Data::Record(_) => Err(VisitorError::UnexpectedRecord(event.clone()).into()),
    }
}

/// Visitor trait.  Implement this to subscribe to the event streams.
/// Each method will be called whenever an event occurs.
#[async_trait]
pub trait EventSubscriber: Send {
    /// `handle_tailer_stdout` and `handle_tailer_stderr` are fired only for tailer stdout/stderr.
    /// Output events produces by the server are sent to `handle_event`.
    async fn handle_tailer_stdout(&mut self, _raw_output: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_tailer_stderr(&mut self, _stderr: &str) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_console_interaction(&mut self, _c: char) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_events(&mut self, _event: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        Ok(())
    }
    async fn handle_command_result(
        &mut self,
        _result: &cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    /// Give the subscriber a chance to react to errors as we start trying to clean up.
    /// They may return another error, which will be incorporated into the end result.
    async fn handle_error(&mut self, _error: &anyhow::Error) -> anyhow::Result<()> {
        Ok(())
    }

    /// Allow the subscriber to do some sort of action once every render cycle.
    async fn tick(&mut self, _tick: &Tick) -> anyhow::Result<()> {
        Ok(())
    }

    /// No more events. Close files, flush buffers etc.
    async fn exit(&mut self) -> anyhow::Result<()> {
        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum VisitorError {
    #[error("Sent an event missing one or more fields: `{0:?}`")]
    MissingField(BuckEvent),
    #[error("Sent an unexpected Record event: `{0:?}`")]
    UnexpectedRecord(BuckEvent),
}
