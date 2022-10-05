/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileName;

/// `~/.buck/buckd/repo-path` directory.
#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{}", path.display())]
pub struct DaemonDir {
    pub path: AbsPathBuf,
}

impl DaemonDir {
    /// Path to `buckd.info` file.
    pub fn buckd_info(&self) -> anyhow::Result<AbsPathBuf> {
        Ok(self.path.join(FileName::new("buckd.info")?))
    }

    /// Path to `buckd.stdout` file.
    pub fn buckd_stdout(&self) -> anyhow::Result<AbsPathBuf> {
        Ok(self.path.join(FileName::new("buckd.stdout")?))
    }

    /// Path to `buckd.stderr` file.
    pub fn buckd_stderr(&self) -> anyhow::Result<AbsPathBuf> {
        Ok(self.path.join(FileName::new("buckd.stderr")?))
    }

    /// Path to `buckd.pid` file.
    pub fn buckd_pid(&self) -> anyhow::Result<AbsPathBuf> {
        Ok(self.path.join(FileName::new("buckd.pid")?))
    }
}
