/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_fs::paths::abs_norm_path::AbsNormPath;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::file_name::FileNameBuf;

use crate::invocation_paths::InvocationPaths;

/// Stable identity used to address one tenant within a shared daemon.
///
/// The invocation working directory is deliberately absent: commands from different directories
/// in the same project and isolation must resolve to the same tenant.
#[derive(Allocative, Clone, Debug, Eq, Hash, PartialEq)]
pub struct TenantKey {
    project_root: AbsNormPathBuf,
    isolation: FileNameBuf,
}

impl TenantKey {
    pub fn from_invocation_paths(paths: &InvocationPaths) -> Self {
        Self {
            project_root: paths.project_root().root().to_buf(),
            isolation: paths.isolation.clone(),
        }
    }

    pub fn project_root(&self) -> &AbsNormPath {
        &self.project_root
    }

    pub fn isolation(&self) -> &FileName {
        &self.isolation
    }
}

/// Description used to validate or construct a tenant.
///
/// It initially contains only the stable address. Tenant lifecycle work will add construction
/// inputs here as their ownership is separated from daemon-wide configuration.
#[derive(Allocative, Clone, Debug, Eq, PartialEq)]
pub struct TenantSpec {
    key: TenantKey,
}

impl TenantSpec {
    pub fn from_invocation_paths(paths: &InvocationPaths) -> Self {
        Self {
            key: TenantKey::from_invocation_paths(paths),
        }
    }

    pub fn key(&self) -> &TenantKey {
        &self.key
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::fs::project::ProjectRoot;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
    use buck2_fs::paths::file_name::FileNameBuf;

    use super::*;
    use crate::invocation_roots::InvocationRoots;

    fn paths(cwd: &str, isolation: &str) -> InvocationPaths {
        let project_root = if cfg!(windows) {
            "C:\\project"
        } else {
            "/project"
        };

        InvocationPaths {
            roots: InvocationRoots {
                project_root: ProjectRoot::new_unchecked(
                    AbsNormPathBuf::try_from(project_root.to_owned())
                        .expect("test project root should be absolute and normalized"),
                ),
                cwd: ProjectRelativePath::new(cwd)
                    .expect("test cwd should be project-relative")
                    .to_buf(),
            },
            isolation: FileNameBuf::try_from(isolation.to_owned())
                .expect("test isolation should be a file name"),
        }
    }

    #[test]
    fn tenant_key_ignores_cwd() {
        let root = TenantSpec::from_invocation_paths(&paths("", "v2"));
        let subdir = TenantSpec::from_invocation_paths(&paths("subdir", "v2"));

        assert_eq!(root.key(), subdir.key());
    }

    #[test]
    fn tenant_key_includes_isolation() {
        let first = TenantSpec::from_invocation_paths(&paths("", "first"));
        let second = TenantSpec::from_invocation_paths(&paths("", "second"));

        assert_ne!(first.key(), second.key());
    }
}
