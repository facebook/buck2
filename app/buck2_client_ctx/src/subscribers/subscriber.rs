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
use buck2_events::BuckEvent;
use dupe::Dupe;

use crate::console_interaction_stream::SuperConsoleToggle;
use crate::subscribers::observer::ErrorObserver;

/// Information about tick timing.
#[derive(Debug, Clone, Dupe)]
pub struct Tick {
    /// The time that the ticker was started.
    pub start_time: Instant,
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

/// Visitor trait.  Implement this to subscribe to the event streams.
/// Each method will be called whenever an event occurs.
#[async_trait]
pub trait EventSubscriber: Send {
    /// Name for debugging, only used for subscribers that have a finalize step.
    fn name(&self) -> &'static str {
        "default"
    }

    /// Fired by the tailer for stdout, or by PartialResultHandler instances that wish to write to
    /// stdout.
    async fn handle_output(&mut self, _raw_output: &[u8]) -> buck2_error::Result<()> {
        Ok(())
    }
    /// Fired by the tailer for stderr.
    async fn handle_tailer_stderr(&mut self, _stderr: &str) -> buck2_error::Result<()> {
        Ok(())
    }
    async fn handle_console_interaction(
        &mut self,
        _c: &Option<SuperConsoleToggle>,
    ) -> buck2_error::Result<()> {
        Ok(())
    }
    async fn handle_events(&mut self, _event: &[Arc<BuckEvent>]) -> buck2_error::Result<()> {
        Ok(())
    }
    async fn handle_command_result(
        &mut self,
        _result: &buck2_cli_proto::CommandResult,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    /// Give the subscriber a chance to react to errors as we start trying to clean up.
    /// They may return another error, which will be incorporated into the end result.
    async fn handle_error(&mut self, _error: &buck2_error::Error) -> buck2_error::Result<()> {
        Ok(())
    }

    /// Allow the subscriber to do some sort of action once every render cycle.
    async fn tick(&mut self, _tick: &Tick) -> buck2_error::Result<()> {
        Ok(())
    }

    /// No more events. Close files, flush buffers etc.
    async fn exit(&mut self) -> buck2_error::Result<()> {
        Ok(())
    }

    fn as_error_observer(&self) -> Option<&dyn ErrorObserver> {
        None
    }

    fn handle_daemon_connection_failure(&mut self, _error: &buck2_error::Error) {}
    fn handle_daemon_started(&mut self, _reason: buck2_data::DaemonWasStartedReason) {}
    fn handle_should_restart(&mut self) {}

    /// Perform final clean up before exiting, upload logs etc.
    async fn finalize(&mut self) -> buck2_error::Result<()> {
        Ok(())
    }
}
