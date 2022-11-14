/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::path::PathBuf;
use std::str::FromStr;

use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::working_dir::WorkingDir;
use serde::Deserialize;
use serde::Serialize;

/// Path arguments for clap which is either absolute or relative to current directory.
///
/// Hides the path, but exposes the function which resolves the path against the current directory.
#[derive(Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PathArg {
    path: PathBuf,
}

impl FromStr for PathArg {
    type Err = <PathBuf as FromStr>::Err;

    fn from_str(s: &str) -> Result<PathArg, Self::Err> {
        Ok(PathArg {
            path: PathBuf::from_str(s)?,
        })
    }
}

impl PathArg {
    /// Resolve path to absolute path using provided current directory.
    pub fn resolve(&self, cwd: &WorkingDir) -> AbsPathBuf {
        cwd.resolve(&self.path)
    }

    pub fn display(&self) -> impl Display + '_ {
        self.path.display()
    }
}
