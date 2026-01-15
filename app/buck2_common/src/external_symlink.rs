/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use allocative::Allocative;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use dupe::Dupe;
use pagable::Pagable;

type Utf8Path = String;

/// Represents a path containing a symlink that resolves to an external path.
/// What path does the symlink resolve to (`abs_target`), and what goes after
/// that (`remaining_path`).
///
/// E.g. foo/bar/file, where foo/bar -> /root, would be represented as:
///      ExternalSymlink { abs_target: "/root", remaining_path: "file" }
#[derive(Debug, Hash, PartialEq, Eq, Clone, Allocative, Pagable)]
pub struct ExternalSymlink {
    /// The external target the symlink resolves to.
    // We can't use AbsPathBuf because there might be "." or ".." in the path
    abs_target: Utf8Path,
    /// What goes after the external target path.
    remaining_path: ForwardRelativePathBuf,
}

impl fmt::Display for ExternalSymlink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_path_buf().display())
    }
}

impl ExternalSymlink {
    pub fn new(
        abs_target: PathBuf,
        remaining_path: ForwardRelativePathBuf,
    ) -> buck2_error::Result<Self> {
        let abs_target = match abs_target.into_os_string().into_string() {
            Ok(string) => string,
            Err(os_string) => {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::Tier0,
                    "Found external symlink that's not utf-8. Lossy representation: {}",
                    os_string.to_string_lossy()
                ));
            }
        };
        Ok(Self {
            abs_target,
            remaining_path,
        })
    }

    pub fn target(&self) -> &Path {
        Path::new(&self.abs_target)
    }

    pub fn remaining_path(&self) -> &ForwardRelativePath {
        &self.remaining_path
    }

    /// Returns the complete path as a [`PathBuf`]
    pub fn to_path_buf(&self) -> PathBuf {
        if !self.remaining_path.is_empty() {
            Path::new(&self.abs_target).join(self.remaining_path.as_str())
        } else {
            Path::new(&self.abs_target).to_owned()
        }
    }

    /// Returns a new `ExternalSymlink` with its target being the full target
    /// of `self` (i.e. `{self.target}/{self.remaining_path}`).
    pub fn with_full_target(self: &Arc<Self>) -> buck2_error::Result<Arc<Self>> {
        if !self.remaining_path.is_empty() {
            Ok(Arc::new(Self::new(
                self.to_path_buf(),
                ForwardRelativePathBuf::default(),
            )?))
        } else {
            Ok(self.dupe())
        }
    }

    /// Returns a new `ExternalSymlink` with `remaining_path` discarded.
    pub fn without_remaining_path(self: &Arc<Self>) -> Arc<Self> {
        if !self.remaining_path.is_empty() {
            Arc::new(Self {
                abs_target: self.abs_target.clone(),
                remaining_path: ForwardRelativePathBuf::default(),
            })
        } else {
            self.dupe()
        }
    }

    pub fn target_str(&self) -> &str {
        &self.abs_target
    }

    /// Given a `path = "[...a]/[...b]"`, and `remaining_path = Some("[...b]")`,
    /// returns `Some("[...a]")`. It returns `None` if `path` doesn't end with
    /// `remaining_path`.
    pub fn fix_source_path<'a>(
        &self,
        path: &'a ForwardRelativePath,
    ) -> Option<&'a ForwardRelativePath> {
        path.strip_suffix_opt(&self.remaining_path)
    }
}
