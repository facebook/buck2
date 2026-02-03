/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_fs::fs_util;
use buck2_fs::paths::abs_norm_path::AbsNormPath;

use crate::execute::blocking::IoRequest;

/// IoRequest we dispatch to the blocking executor to clear output paths.
pub struct CleanOutputPaths {
    pub paths: Vec<ProjectRelativePathBuf>,
}

impl CleanOutputPaths {
    pub fn clean<'a>(
        paths: impl IntoIterator<Item = &'a ProjectRelativePath>,
        fs: &'a ProjectRoot,
    ) -> buck2_error::Result<()> {
        for path in paths {
            cleanup_path(fs, path)
                .with_buck_error_context(|| format!("Error cleaning up output path `{path}`"))?;
        }
        Ok(())
    }
}

#[cfg(unix)]
fn tag_environment_error(error: buck2_error::Error) -> buck2_error::Error {
    error
}

#[cfg(windows)]
fn tag_environment_error(error: buck2_error::Error) -> buck2_error::Error {
    use buck2_error::ErrorTag;
    if error.has_tag(ErrorTag::IoWindowsSharingViolation)
        | error.has_tag(ErrorTag::IoPermissionDenied)
    {
        error
            .tag([ErrorTag::IoMaterializerFileBusy])
            .context("Binary being executed, please close the process first")
    } else {
        error
    }
}

use buck2_fs::fs_util::IoError;
fn tag_cleanup_path_env_error(res: Result<(), IoError>) -> buck2_error::Result<()> {
    res.map_err(buck2_error::Error::from)
        .map_err(tag_environment_error)
}

#[tracing::instrument(level = "debug", skip(fs), fields(path = %path))]
pub fn cleanup_path(fs: &ProjectRoot, path: &ProjectRelativePath) -> buck2_error::Result<()> {
    let path = fs.resolve(path);

    // This will remove the path if it exists.
    tag_cleanup_path_env_error(fs_util::remove_all(&path))?;

    let mut path: &AbsNormPath = &path;

    // Be aware of T85589819 - the parent directory might already exist, but as a _file_.  It might
    // be even worse, it might be 2 parents up, which will cause create_dir to fail when we try to
    // execute. So, we walk up the tree until we either find a dir we're happy with, or a file we
    // can delete. It's safe to delete this file because we know it doesn't overlap with a current
    // output, or that check would have failed, so it must be a stale file.
    loop {
        path = match path.parent() {
            Some(path) => path,
            None => {
                return Err(buck2_error!(
                    buck2_error::ErrorTag::CleanOutputs,
                    "Internal Error: reached root before finding a directory that exists!"
                ));
            }
        };

        match fs_util::symlink_metadata_if_exists(path) {
            Ok(Some(m)) => {
                if m.is_dir() {
                    // It's a dir, no need to go further, and no need to delete.
                    tracing::trace!(path = %path, "skip (is dir)");
                } else {
                    // There was a file or a symlink, so it's safe to delete and then we can exit
                    // because we'll be able to create a dir here.
                    tracing::trace!(path = %path, "remove_file");
                    tag_cleanup_path_env_error(fs_util::remove_file(path))?;
                }
                return Ok(());
            }
            Ok(None) if cfg!(unix) => {
                // If we get ENOENT that guarantees there is no file on the path. If there was
                // one, we would get ENOTDIR. TODO (T123279320) This probably works on Windows,
                // but it wasn't tested there.
                //
                // On non-Unix we don't have this optimization. Recursing all the way up
                // until we find the first dir (or file to delete) is fine. There will
                // eventually be *a* directory (at buck-out, then another one at the empty
                // directory, which is our cwd, and should exist by now).
                tracing::trace!(path = %path, "skip (ENOENT)");
                return Ok(());
            }
            Ok(None) => {
                tracing::trace!(path = %path, "continue (ENOENT)");
            }
            Err(e) => {
                // Continue going up. Eventually we should reach the output directory, which should
                // exist.
                tracing::trace!(path = %path, "continue (error: {:?})", e);
            }
        }
    }
}

impl IoRequest for CleanOutputPaths {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
        Self::clean(self.paths.iter().map(AsRef::as_ref), project_fs)
    }
}
