/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! Defines utilities to obtain the basic paths for buck2 client and the daemon.
//!

use std::borrow::Cow;

use anyhow::anyhow;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;

use crate::roots::Roots;

#[derive(Clone)]
pub(crate) struct Paths {
    pub(crate) roots: Roots,

    /// The isolation dir is a dir relative path used to create unique directories for
    /// all on-disk state relating to a daemon. This allows multiple daemons to run in
    /// the same project root.
    ///
    /// The daemon metadata directory is post-fixed with the isolation prefix
    /// (i.e `$HOME/.buck/buckd/<projectroot>/<isolationdir>`).
    /// The buck-out is `<projectroot>/buck-out/<isolationdir>/`
    ///
    /// Any on-disk state from the daemon (including build outputs and similar) should only
    /// be written or read from directories that include this component.
    ///
    /// This form of isolation is currently supported primarily for two uses:
    /// 1. testing - it allows us to run isolated daemons on a project for tests. This is
    /// particularly useful to allow a test in a project to recursively invoke buck, but also
    /// useful to write tests against a project's macros and rules and using a project's real
    /// configuration.
    /// 2. generally to support recursive buck invocations. while our ideal may be that these
    /// eventually are not allowed, the most pragmatic approach currently is to support them
    /// but push them into isolated, temporary daemons.
    pub(crate) isolation: FileNameBuf,
}

impl Paths {
    pub(crate) fn daemon_dir(&self) -> anyhow::Result<AbsPathBuf> {
        #[cfg(windows)]
        let root_relative: Cow<ForwardRelativePath> = {
            use buck2_core::fs::paths::ForwardRelativePathNormalizer;

            // Get drive letter, network share name, etc.
            // Network share contains '\' therefore it needs to be normalized.
            let prefix = self.roots.project_root.windows_prefix()?;
            let stripped_path = ForwardRelativePathNormalizer::normalize_path(
                self.roots.project_root.strip_windows_prefix()?,
            )?;
            Cow::Owned(ForwardRelativePathNormalizer::normalize_path(&prefix)?.join(stripped_path))
        };
        #[cfg(not(windows))]
        let root_relative: Cow<ForwardRelativePath> = self
            .roots
            .project_root
            .strip_prefix(&AbsPath::unchecked_new("/"))?;
        // TODO(cjhopman): We currently place all buckd info into a directory owned by the user.
        // This is broken when multiple users try to share the same checkout.
        //
        // **This is different than the behavior of buck1.**
        //
        // In buck1, the buck daemon is shared across users. Due to the fact that `buck run`
        // will run whatever command is returned by the daemon, buck1 has a privilege escalation
        // vulnerability.
        //
        // There's a couple ways we could resolve this:
        // 1. Use a shared .buckd information directory and have the client verify the identity of
        // the server before doing anything with it. If the identity is different, kill it and
        // start a new one.
        // 2. Keep user-owned .buckd directory, use some other mechanism to move ownership of
        // output directories between different buckd instances.
        let home =
            dirs::home_dir().ok_or_else(|| anyhow!("Expected a HOME directory to be available"))?;
        let paths = AbsPath::new(&home)
            .map_err(|e| anyhow!("Expected an absolute HOME directory. {}", e))?;

        let prefix = ".buck/buckd";

        let mut ret = AbsPathBuf::with_capacity(
            paths.as_os_str().len()
                + 1
                + prefix.len()
                + 1
                + root_relative.as_str().len()
                + 1
                + self.isolation.as_str().len(),
            paths,
        );

        ret.push(ForwardRelativePath::unchecked_new(".buck/buckd"));
        ret.push(root_relative.as_ref());
        ret.push(&self.isolation);

        Ok(ret)
    }

    pub(crate) fn cell_root(&self) -> &AbsPath {
        &self.roots.cell_root
    }

    pub(crate) fn project_root(&self) -> &AbsPath {
        &self.roots.project_root
    }

    pub(crate) fn log_dir(&self) -> AbsPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("log"))
    }

    pub(crate) fn dice_dump_dir(&self) -> AbsPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("dice_dump"))
    }

    pub(crate) fn buck_out_dir(&self) -> ProjectRelativePathBuf {
        ProjectRelativePath::unchecked_new("buck-out").join(&self.isolation)
    }

    pub(crate) fn buck_out_path(&self) -> AbsPathBuf {
        self.roots.project_root.join(&self.buck_out_dir())
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;

    use buck2_core::fs::paths::AbsPath;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::paths::FileNameBuf;
    use buck2_core::fs::paths::ForwardRelativePath;
    use buck2_core::fs::project::ProjectRelativePathBuf;

    use crate::paths::Paths;
    use crate::roots::Roots;

    #[test]
    fn test_paths() {
        let cell_root = if cfg!(windows) {
            "C:\\my\\project\\root\\cell"
        } else {
            "/my/project/root/cell"
        };
        let project_root = if cfg!(windows) {
            "C:\\my\\project"
        } else {
            "/my/project"
        };
        let paths = Paths {
            roots: Roots {
                cell_root: AbsPathBuf::unchecked_new(cell_root.to_owned()),
                project_root: AbsPathBuf::unchecked_new(project_root.to_owned()),
            },
            isolation: FileNameBuf::unchecked_new("isolation".to_owned()),
        };

        let expected_path = if cfg!(windows) {
            ".buck\\buckd\\C\\my\\project\\isolation"
        } else {
            ".buck/buckd/my/project/isolation"
        };
        assert_eq!(
            paths.daemon_dir().unwrap().as_os_str(),
            AbsPathBuf::try_from(
                dirs::home_dir().expect("Expected a HOME directory to be available")
            )
            .expect("Expected an absolute HOME directory")
            .join_normalized(ForwardRelativePath::unchecked_new(expected_path))
            .unwrap()
            .as_os_str()
        );

        let expected_path = if cfg!(windows) {
            "C:\\my\\project\\root\\cell"
        } else {
            "/my/project/root/cell"
        };
        assert_eq!(paths.cell_root().as_os_str(), OsStr::new(expected_path));
        let expected_path = if cfg!(windows) {
            "C:\\my\\project"
        } else {
            "/my/project"
        };
        assert_eq!(
            paths.project_root().as_os_str(),
            AbsPath::unchecked_new(expected_path).as_os_str()
        );

        assert_eq!(
            paths.buck_out_dir(),
            ProjectRelativePathBuf::unchecked_new("buck-out/isolation".to_owned())
        );
        let expected_path = if cfg!(windows) {
            "C:\\my\\project\\buck-out\\isolation"
        } else {
            "/my/project/buck-out/isolation"
        };
        assert_eq!(paths.buck_out_path().as_os_str(), OsStr::new(expected_path));

        let expected_path = if cfg!(windows) {
            "C:\\my\\project\\buck-out\\isolation\\log"
        } else {
            "/my/project/buck-out/isolation/log"
        };
        assert_eq!(paths.log_dir().as_os_str(), OsStr::new(expected_path));
        let expected_path = if cfg!(windows) {
            "C:\\my\\project\\buck-out\\isolation\\dice_dump"
        } else {
            "/my/project/buck-out/isolation/dice_dump"
        };
        assert_eq!(paths.dice_dump_dir().as_os_str(), OsStr::new(expected_path));
    }
}
