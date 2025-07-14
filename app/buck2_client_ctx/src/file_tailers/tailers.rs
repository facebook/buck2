/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::daemon_dir::DaemonDir;
use tokio::sync::mpsc;
use tokio::sync::mpsc::UnboundedReceiver;

use crate::events_ctx::FileTailerEvent;
use crate::file_tailers::tailer::FileTailer;
use crate::file_tailers::tailer::StdoutOrStderr;

pub struct FileTailers {
    _stdout_tailer: Option<FileTailer>,
    _stderr_tailer: Option<FileTailer>,
    pub(crate) stream: Option<UnboundedReceiver<FileTailerEvent>>,
}

impl FileTailers {
    pub fn new(daemon_dir: &DaemonDir) -> buck2_error::Result<Self> {
        let (tx, rx) = mpsc::unbounded_channel();
        let stdout_tailer = FileTailer::tail_file(
            daemon_dir.buckd_stdout(),
            tx.clone(),
            StdoutOrStderr::Stdout,
        )?;
        let stderr_tailer =
            FileTailer::tail_file(daemon_dir.buckd_stderr(), tx, StdoutOrStderr::Stderr)?;
        let this = Self {
            _stdout_tailer: Some(stdout_tailer),
            _stderr_tailer: Some(stderr_tailer),
            stream: Some(rx),
        };
        Ok(this)
    }

    pub fn empty() -> FileTailers {
        FileTailers {
            _stdout_tailer: None,
            _stderr_tailer: None,
            // Empty stream.
            stream: None,
        }
    }

    pub async fn recv(&mut self) -> Option<FileTailerEvent> {
        if let Some(stream) = self.stream.as_mut() {
            stream.recv().await
        } else {
            None
        }
    }

    pub fn stop_reading(&mut self) -> Option<UnboundedReceiver<FileTailerEvent>> {
        // dropping the tailers shuts them down and closes the stream.
        drop(self._stderr_tailer.take());
        drop(self._stdout_tailer.take());

        self.stream.take()
    }
}
