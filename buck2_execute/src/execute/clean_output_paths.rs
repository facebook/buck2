/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;

use crate::execute::blocking::IoRequest;

/// IoRequest we dispatch to the blocking executor to clear output paths.
pub struct CleanOutputPaths {
    pub paths: Vec<ProjectRelativePathBuf>,
}

impl CleanOutputPaths {
    pub fn clean<'a>(
        paths: impl Iterator<Item = &'a ProjectRelativePath>,
        fs: &'a ProjectRoot,
    ) -> anyhow::Result<()> {
        for path in paths {
            cleanup_path(fs, path)
                .with_context(|| format!("Error cleaning up output path `{}`", path))?;
        }
        Ok(())
    }
}

pub fn cleanup_path(fs: &ProjectRoot, mut path: &ProjectRelativePath) -> anyhow::Result<()> {
    fs.remove_path_recursive(path)?;

    // Be aware of T85589819 - the parent directory might already exist, but as a _file_.  It might
    // be even worse, it might be 2 parents up, which will cause create_dir to fail when we try to
    // execute. So, we walk up the tree until we either find a dir we're happy with, or a file we
    // can delete. It's safe to delete this file because we know it doesn't overlap with a current
    // output, or that check would have failed, so it must be a stale file.
    loop {
        path = match path.parent() {
            Some(path) => path,
            None => {
                return Err(anyhow::anyhow!(
                    "Internal Error: reached root before finding a directory that exists!"
                ));
            }
        };

        match fs.resolve(path).symlink_metadata() {
            Ok(m) => {
                if m.is_dir() {
                    // It's a dir, no need to go further, and no need to delete.
                } else {
                    // There was a file , so it's safe to delete and then we can exit because we'll
                    // be able to create a dir here.
                    fs.remove_path_recursive(path)?;
                }
                return Ok(());
            }
            Err(e) => {
                #[cfg(unix)]
                {
                    use std::io;

                    // If we get ENOENT that guarantees there is no file on the path. If there was
                    // one, we would get ENOTDIR. TODO (T123279320) This probably works on Windows,
                    // but it wasn't tested there.
                    let is_enoent = e.kind() == io::ErrorKind::NotFound;

                    if is_enoent {
                        return Ok(());
                    }
                }

                #[cfg(not(unix))]
                {
                    // On non-Unix we don't have the optimization above. Recursing all the way up
                    // until we find the first dir (or file to delete) is fine. There will
                    // eventually be *a* directory (at buck-out, then another one at the empty
                    // directory, which is our cwd, and should exist by now).
                    let _e = e;
                }

                // Continue going up. Eventually we should reach the output directory, which should
                // exist.
            }
        }
    }
}

impl IoRequest for CleanOutputPaths {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> anyhow::Result<()> {
        Self::clean(self.paths.iter().map(AsRef::as_ref), project_fs)
    }
}
