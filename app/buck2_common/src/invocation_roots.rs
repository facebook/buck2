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

use allocative::Allocative;
use anyhow::Context as _;
use buck2_core::buck2_env;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::project::ProjectRoot;
use once_cell::sync::Lazy;

#[derive(Debug, buck2_error::Error)]
enum BuckCliError {
    #[error(
        "Couldn't find a buck project root for directory `{}`. Expected to find a .buckconfig file.", _0.display()
    )]
    NoBuckRoot(PathBuf),
}

#[derive(Clone, Allocative)]
pub struct InvocationRoots {
    pub cell_root: AbsNormPathBuf,
    pub project_root: ProjectRoot,
}

impl InvocationRoots {
    pub fn common_buckd_dir(&self) -> anyhow::Result<AbsNormPathBuf> {
        Ok(home_buck_dir()?.join(FileName::unchecked_new("buckd")))
    }

    pub fn paranoid_info_path(&self) -> anyhow::Result<AbsPathBuf> {
        // Used in tests
        if let Some(p) = buck2_env!("BUCK2_PARANOID_PATH")? {
            return AbsPathBuf::try_from(p.to_owned());
        }

        Ok(self
            .common_buckd_dir()?
            .join(FileName::new("paranoid.info")?)
            .into_abs_path_buf())
    }
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
/// We also look for .buckroot files, and if we find one of them, we don't traverse further upwards.
/// The contents of the .buckroot file is entirely ignored.
///
/// Doing this without those requirements (i.e. doing it correctly), would require us to
/// parse the buckconfig files (including all file includes). It's unclear if we'll ever
/// do that within the buckd client dir.
pub fn find_invocation_roots(from: &Path) -> anyhow::Result<InvocationRoots> {
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

        if curr.join(".buckroot").exists() {
            break;
        }
    }
    match (project_root, cell_root) {
        (Some(project_root), Some(cell_root)) => Ok(InvocationRoots {
            cell_root: AbsNormPathBuf::try_from(cell_root)?,
            project_root: ProjectRoot::new(AbsNormPathBuf::try_from(project_root)?)?,
        }),
        _ => Err(BuckCliError::NoBuckRoot(from.to_owned()).into()),
    }
}

/// `~/.buck`.
/// TODO(cjhopman): We currently place all buckd info into a directory owned by the user.
/// This is broken when multiple users try to share the same checkout.
///
/// **This is different than the behavior of buck1.**
///
/// In buck1, the buck daemon is shared across users. Due to the fact that `buck run`
/// will run whatever command is returned by the daemon, buck1 has a privilege escalation
/// vulnerability.
///
/// There's a couple ways we could resolve this:
/// 1. Use a shared .buckd information directory and have the client verify the identity of
/// the server before doing anything with it. If the identity is different, kill it and
/// start a new one.
/// 2. Keep user-owned .buckd directory, use some other mechanism to move ownership of
/// output directories between different buckd instances.
pub(crate) fn home_buck_dir() -> anyhow::Result<&'static AbsNormPath> {
    fn find_dir() -> anyhow::Result<AbsNormPathBuf> {
        let home = dirs::home_dir().context("Expected a HOME directory to be available")?;
        let home = AbsNormPathBuf::new(home).context("Expected an absolute HOME directory")?;
        Ok(home.join(FileName::new(".buck")?))
    }

    static DIR: Lazy<buck2_error::Result<AbsNormPathBuf>> =
        Lazy::new(|| find_dir().map_err(buck2_error::Error::from));

    Ok(&Lazy::force(&DIR).as_ref().map_err(dupe::Dupe::dupe)?)
}
