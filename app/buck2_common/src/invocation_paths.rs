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

use std::borrow::Cow;

use allocative::Allocative;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

use crate::daemon_dir::DaemonDir;
use crate::invocation_roots::InvocationRoots;

#[derive(Clone, Allocative)]
pub struct InvocationPaths {
    pub roots: InvocationRoots,

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
    pub isolation: FileNameBuf,
}

impl InvocationPaths {
    pub fn daemon_dir(&self) -> anyhow::Result<DaemonDir> {
        #[cfg(windows)]
        let root_relative: Cow<ForwardRelativePath> = {
            use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathNormalizer;

            // Get drive letter, network share name, etc.
            // Network share contains '\' therefore it needs to be normalized.
            let prefix = self.roots.project_root.root().windows_prefix()?;
            let stripped_path = ForwardRelativePathNormalizer::normalize_path(
                self.roots.project_root.root().strip_windows_prefix()?,
            )?;
            Cow::Owned(ForwardRelativePathNormalizer::normalize_path(&prefix)?.join(stripped_path))
        };
        #[cfg(not(windows))]
        let root_relative: Cow<ForwardRelativePath> = self
            .roots
            .project_root
            .root()
            .strip_prefix(AbsNormPath::new("/")?)?;

        let path = self
            .roots
            .common_buckd_dir()?
            .join(root_relative.as_ref())
            .join(&self.isolation);

        Ok(DaemonDir { path })
    }

    pub fn cell_root(&self) -> &AbsNormPath {
        &self.roots.cell_root
    }

    pub fn project_root(&self) -> &ProjectRoot {
        &self.roots.project_root
    }

    pub fn log_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("log"))
    }

    pub fn tmp_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("tmp"))
    }

    pub fn re_logs_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("re_logs"))
    }

    pub fn build_count_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("build_count"))
    }

    pub fn dice_dump_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("dice_dump"))
    }

    pub fn buck_out_dir_prefix() -> &'static ProjectRelativePath {
        ProjectRelativePath::unchecked_new("buck-out")
    }

    pub fn buck_out_dir(&self) -> ProjectRelativePathBuf {
        Self::buck_out_dir_prefix().join(&self.isolation)
    }

    pub fn buck_out_path(&self) -> AbsNormPathBuf {
        self.roots.project_root.root().join(self.buck_out_dir())
    }

    /// Directory containing on-disk cache
    pub fn cache_dir(&self) -> ProjectRelativePathBuf {
        self.buck_out_dir()
            .join(ForwardRelativePath::unchecked_new("cache"))
    }

    /// Temporary directory for paranoid downloads.
    pub fn paranoid_cache_dir(&self) -> ProjectRelativePathBuf {
        self.buck_out_dir()
            .join(ForwardRelativePath::unchecked_new("paranoid"))
    }

    pub fn cache_dir_path(&self) -> AbsNormPathBuf {
        self.roots.project_root.root().join(self.cache_dir())
    }

    /// Subdirectory of `cache_dir` responsible for storing materializer state
    pub fn materializer_state_path(&self) -> AbsNormPathBuf {
        self.cache_dir_path()
            .join(self.materializer_state_dir_name())
    }

    /// This is used by the forkserver to write the miniperf wrapper binary (if used), as well as
    /// temporary files used by miniperf. We put this in buck-out because that directory gets
    /// allowlisted for execution (because we write lots of tools there).
    pub fn forkserver_state_dir(&self) -> AbsNormPathBuf {
        self.buck_out_path()
            .join(ForwardRelativePath::unchecked_new("forkserver"))
    }

    pub fn materializer_state_dir_name(&self) -> &FileName {
        FileName::unchecked_new("materializer_state")
    }

    pub fn valid_cache_dirs(&self) -> Vec<&FileName> {
        vec![self.materializer_state_dir_name()]
    }
}

#[cfg(test)]
mod tests {
    use std::ffi::OsStr;

    use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_core::fs::paths::file_name::FileNameBuf;
    use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;

    use crate::invocation_paths::InvocationPaths;
    use crate::invocation_roots::InvocationRoots;

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
        let paths = InvocationPaths {
            roots: InvocationRoots {
                cell_root: AbsNormPathBuf::try_from(cell_root.to_owned()).unwrap(),
                project_root: ProjectRoot::new_unchecked(
                    AbsNormPathBuf::try_from(project_root.to_owned()).unwrap(),
                ),
            },
            isolation: FileNameBuf::unchecked_new("isolation"),
        };

        let expected_path = if cfg!(windows) {
            ".buck\\buckd\\C\\my\\project\\isolation"
        } else {
            ".buck/buckd/my/project/isolation"
        };
        assert_eq!(
            paths.daemon_dir().unwrap().path.as_os_str(),
            AbsNormPathBuf::try_from(
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
            paths.project_root().root().as_os_str(),
            AbsNormPath::new(expected_path).unwrap().as_os_str()
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

        assert_eq!(
            paths.cache_dir(),
            ProjectRelativePathBuf::unchecked_new("buck-out/isolation/cache".to_owned())
        );

        let expected_path = if cfg!(windows) {
            "C:\\my\\project\\buck-out\\isolation\\cache\\materializer_state"
        } else {
            "/my/project/buck-out/isolation/cache/materializer_state"
        };
        assert_eq!(
            paths.materializer_state_path().as_os_str(),
            OsStr::new(expected_path),
        );
    }
}
