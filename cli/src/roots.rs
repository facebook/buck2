/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use buck2_core::fs::paths::AbsPathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
enum BuckCliError {
    #[error(
        "Couldn't find a buck project root for directory `{0}`. Expected to find a .buckconfig file."
    )]
    NoBuckRoot(PathBuf),
}

#[derive(Clone)]
pub(crate) struct Roots {
    pub cell_root: AbsPathBuf,
    pub project_root: AbsPathBuf,
}

/// This finds the cell root and the project root.
///
/// This uses a rather liberal definition of "roots". It traverses the directory and
/// its parents looking for all .buckconfig files, the closest one (i.e. with the longest
/// path) will be detected as the "cell root" and the furthest one (with the shortest
/// path) will be detected as the "project root".
///
/// This definition requires that (1) every cell has a .buckconfig file (to make the "cell
/// root" correct) and (2) that the cell's buckconfig actually references the outermost
/// .buckconfig directory as one of its cells, which in practice means you can't stick a
/// buck cell/project within another one without them referencing each other (or not
/// running buck commands within the inner one).
///
/// Doing this without those requirements (i.e. doing it correctly), would require us to
/// parse the buckconfig files (including all file includes). It's unclear if we'll ever
/// do that within the buckd client dir.
pub(crate) fn find_roots(from: &Path) -> anyhow::Result<Roots> {
    let mut cell_root = None;
    let mut project_root = None;

    let home_dir = dirs::home_dir();
    for curr in from.ancestors() {
        if curr.join(".buckconfig").exists() {
            // Do not allow /home/{unixname}, /home or / to be a cell,
            // and /home/{unixname}/.buckconfig is used for config override
            if let Some(home_dir_path) = &home_dir {
                if home_dir_path == curr {
                    break;
                }
            }
            if cell_root.is_none() {
                cell_root = Some(curr.to_owned());
            }
            project_root = Some(curr.to_owned());
        }
    }
    match (project_root, cell_root) {
        (Some(project_root), Some(cell_root)) => Ok(Roots {
            cell_root: AbsPathBuf::try_from(cell_root)?,
            project_root: AbsPathBuf::try_from(project_root)?,
        }),
        _ => Err(BuckCliError::NoBuckRoot(from.to_owned()).into()),
    }
}

/// Finds the cell root and the project root starting at the cwd.
/// For more details, see `find_roots`.
pub(crate) fn find_current_roots() -> anyhow::Result<Roots> {
    find_roots(&std::env::current_dir()?)
}
