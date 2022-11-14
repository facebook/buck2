/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;

/// `~/.buck/buckd/repo-path` directory.
#[derive(Debug, Clone, derive_more::Display)]
#[display(fmt = "{}", path.display())]
pub struct DaemonDir {
    pub path: AbsNormPathBuf,
}

impl DaemonDir {
    /// Path to `buckd.info` file.
    pub fn buckd_info(&self) -> AbsNormPathBuf {
        self.path.join(FileName::new("buckd.info").unwrap())
    }

    /// Path to `buckd.stdout` file.
    pub fn buckd_stdout(&self) -> AbsNormPathBuf {
        self.path.join(FileName::new("buckd.stdout").unwrap())
    }

    /// Path to `buckd.stderr` file.
    pub fn buckd_stderr(&self) -> AbsNormPathBuf {
        self.path.join(FileName::new("buckd.stderr").unwrap())
    }

    /// Path to `buckd.pid` file.
    pub fn buckd_pid(&self) -> AbsNormPathBuf {
        self.path.join(FileName::new("buckd.pid").unwrap())
    }
}
