/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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
    pub(crate) stream: UnboundedReceiver<FileTailerEvent>,
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
            stream: rx,
        };
        Ok(this)
    }

    pub fn empty() -> FileTailers {
        FileTailers {
            _stdout_tailer: None,
            _stderr_tailer: None,
            // Empty stream.
            stream: mpsc::unbounded_channel().1,
        }
    }

    pub fn stop_reading(self) -> UnboundedReceiver<FileTailerEvent> {
        // by dropping the tailers, they shut themselves down.
        self.stream
    }
}
