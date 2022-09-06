/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use gazebo::dupe::Dupe;

/// Represents a path containing a symlink that resolves to an external path.
/// What path does the symlink resolve to (`abs_target`), and what goes after
/// that (`remaining_path`).
///
/// E.g. foo/bar/file, where foo/bar -> /root, would be represented as:
///      ExternalSymlink { abs_target: "/root", remaining_path: "file" }
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub struct ExternalSymlink {
    /// The external target the symlink resolves to.
    // We can't use AbsPathBuf because there might be "." or ".." in the path
    abs_target: PathBuf,
    /// What goes after the external target path.
    remaining_path: Option<ForwardRelativePathBuf>,
}

impl fmt::Display for ExternalSymlink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_path_buf().to_string_lossy())
    }
}

impl ExternalSymlink {
    pub fn new(abs_target: PathBuf, remaining_path: Option<ForwardRelativePathBuf>) -> Self {
        Self {
            abs_target,
            remaining_path,
        }
    }

    pub fn target(&self) -> &Path {
        self.abs_target.as_ref()
    }

    pub fn remaining_path(&self) -> Option<&ForwardRelativePath> {
        self.remaining_path.as_ref().map(|p| p.as_ref())
    }

    /// Returns the complete path as a [`PathBuf`]
    pub fn to_path_buf(&self) -> PathBuf {
        match &self.remaining_path {
            Some(p) => self.abs_target.join(p.as_str()),
            None => self.abs_target.clone(),
        }
    }

    /// Returns a new `ExternalSymlink` with its target being the full target
    /// of `self` (i.e. `{self.target}/{self.remaining_path}`).
    pub fn with_full_target(self: &Arc<Self>) -> Arc<Self> {
        if self.remaining_path.is_some() {
            Arc::new(Self::new(self.to_path_buf(), None))
        } else {
            self.dupe()
        }
    }

    /// Returns a new `ExternalSymlink` with `remaining_path` discarded.
    pub fn without_remaining_path(self: &Arc<Self>) -> Arc<Self> {
        if self.remaining_path.is_some() {
            Arc::new(Self::new(self.abs_target.clone(), None))
        } else {
            self.dupe()
        }
    }

    /// Given a `path = "[...a]/[...b]"`, and `remaining_path = Some("[...b]")`,
    /// returns `Some("[...a]")`. It returns `None` if `path` doesn't end with
    /// `remaining_path`.
    pub fn fix_source_path<'a>(
        &self,
        path: &'a ForwardRelativePath,
    ) -> Option<&'a ForwardRelativePath> {
        if let Some(remaining) = self.remaining_path() {
            path.as_str()
                .strip_suffix(remaining.as_str())
                .map(|p| p.strip_suffix('/').unwrap_or(p))
                .map(ForwardRelativePath::unchecked_new)
        } else {
            Some(path)
        }
    }
}
