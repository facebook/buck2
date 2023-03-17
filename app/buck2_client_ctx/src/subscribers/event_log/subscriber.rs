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
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use buck2_events::BuckEvent;

use crate::cleanup_ctx::AsyncCleanupContext;
use crate::subscribers::event_log::write::WriteEventLog;
use crate::subscribers::subscriber::EventSubscriber;
use crate::subscribers::subscriber::Tick;

/// This EventLog lets us to events emitted by Buck and log them to a file. The events are
/// serialized as JSON and logged one per line.
pub(crate) struct EventLog {
    writer: WriteEventLog,
}

impl EventLog {
    pub(crate) fn new(
        logdir: AbsNormPathBuf,
        working_dir: WorkingDir,
        extra_path: Option<AbsPathBuf>,
        sanitized_argv: Vec<String>,
        async_cleanup_context: AsyncCleanupContext,
        command_name: String,
    ) -> anyhow::Result<EventLog> {
        Ok(Self {
            writer: WriteEventLog::new(
                logdir,
                working_dir,
                extra_path,
                sanitized_argv,
                async_cleanup_context,
                command_name,
            )?,
        })
    }
}

#[async_trait]
impl EventSubscriber for EventLog {
    async fn handle_events(&mut self, events: &[Arc<BuckEvent>]) -> anyhow::Result<()> {
        self.writer.write_events(events).await
    }

    async fn handle_command_result(
        &mut self,
        result: &buck2_cli_proto::CommandResult,
    ) -> anyhow::Result<()> {
        self.writer.write_result(result).await
    }

    /// Flush all log files during on tick to avoid buffering data in memory which we might lose if
    /// we hit an error.
    async fn tick(&mut self, _tick: &Tick) -> anyhow::Result<()> {
        self.writer.flush_files().await
    }

    async fn exit(&mut self) -> anyhow::Result<()> {
        self.writer.exit().await
    }
}
