/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::io;
use std::path::Components;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use buck2_core::fs::paths::AbsPath;
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

    pub fn from_disk<P: AsRef<AbsPath>>(relpath: &ForwardRelativePath, root: P) -> Option<Self> {
        fn external_sym(
            path: &mut PathBuf,
            remaining_path: &mut Components,
        ) -> io::Result<Option<PathBuf>> {
            assert!(path.is_absolute());

            for c in remaining_path {
                // TODO(nga): this code does not make sense: we push one component here,
                //   and we pop it 10 lines down, and on the next iteration we push next component?
                path.push(c);
                if !path.symlink_metadata()?.file_type().is_symlink() {
                    continue;
                }

                // We got a symlink, does it point to an absolute path?
                let dest = path.read_link()?;
                if dest.is_absolute() {
                    return Ok(Some(dest));
                }

                // It points to a relative path, we'll have to traverse it
                let mut dest_remaining = dest.components();
                path.pop(); // pop symlink name

                // Traverse dest: does it hit an absolute path?
                if let Some(mut extdest) = external_sym(path, &mut dest_remaining)? {
                    // Absolute path found! Push `dest_remaining` because we
                    // actually point to `{extdest}/{dest_remaining}`
                    let dest_remaining = dest_remaining.as_path();
                    if !dest_remaining.as_os_str().is_empty() {
                        extdest.push(dest_remaining);
                    }
                    return Ok(Some(extdest));
                }
            }
            Ok(None)
        }

        // If x -> y -> /external, we simply ignore the existence of y, as
        // if x -> /external directly; in the future we want to fix that
        // TODO: change ExternalSymlink to include information about the
        // followed symlinks (we can do that with a `next` attr?)

        let root = root.as_ref();
        let relpath = Path::new(relpath.as_str());
        let mut remaining_path = relpath.components();

        external_sym(&mut root.to_path_buf(), &mut remaining_path)
            .ok()?
            .map(|dest| {
                let remaining_path = remaining_path.as_path();
                let remaining_path = if !remaining_path.as_os_str().is_empty() {
                    Some(
                        ForwardRelativePath::new(remaining_path)
                            .expect("cannot fail: Path was constructed from ForwardRelativePath")
                            .to_owned(),
                    )
                } else {
                    None
                };
                Self::new(dest, remaining_path)
            })
    }
}

#[cfg(all(test, unix))]
mod tests {
    use std::fs;
    use std::os::unix;

    use buck2_core::fs::paths::AbsPath;
    use buck2_core::fs::paths::ForwardRelativePath;
    use buck2_core::fs::paths::ForwardRelativePathBuf;
    use tempfile::TempDir;

    use crate::external_symlink::ExternalSymlink;

    #[test]
    fn test_from_disk_not_symlink() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        fs::write(t.path().join("x"), "xx")?;

        assert_eq!(
            None,
            ExternalSymlink::from_disk(ForwardRelativePath::new("x")?, AbsPath::new(t.path())?)
        );

        Ok(())
    }

    #[test]
    fn test_from_disk_local_rel() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        fs::write(t.path().join("y"), "yy")?;

        unix::fs::symlink("y", t.path().join("x"))?;

        assert_eq!(
            None,
            ExternalSymlink::from_disk(ForwardRelativePath::new("y")?, AbsPath::new(t.path())?)
        );

        Ok(())
    }

    #[test]
    fn test_from_disk_local_abs() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        fs::write(t.path().join("y"), "yy")?;

        unix::fs::symlink(t.path().join("y"), t.path().join("x"))?;

        assert_eq!(
            None,
            ExternalSymlink::from_disk(ForwardRelativePath::new("y")?, AbsPath::new(t.path())?)
        );

        Ok(())
    }

    #[test]
    fn test_from_disk_external() -> anyhow::Result<()> {
        let external = TempDir::new()?;

        let t = TempDir::new()?;

        fs::create_dir(t.path().join("x"))?;

        unix::fs::symlink(external.path(), t.path().join("x/y"))?;

        assert_eq!(
            Some(ExternalSymlink {
                abs_target: external.path().to_path_buf(),
                remaining_path: None
            }),
            ExternalSymlink::from_disk(ForwardRelativePath::new("x/y")?, AbsPath::new(t.path())?)
        );

        Ok(())
    }

    #[test]
    fn test_from_disk_external_with_suffix() -> anyhow::Result<()> {
        let external = TempDir::new()?;

        let t = TempDir::new()?;

        fs::create_dir(t.path().join("x"))?;

        unix::fs::symlink(external.path(), t.path().join("x/y"))?;

        assert_eq!(
            Some(ExternalSymlink {
                abs_target: external.path().to_path_buf(),
                remaining_path: Some(ForwardRelativePathBuf::new("z/w".to_owned())?),
            }),
            ExternalSymlink::from_disk(
                ForwardRelativePath::new("x/y/z/w")?,
                AbsPath::new(t.path())?
            )
        );

        Ok(())
    }

    #[test]
    fn test_from_disk_local_rel_broken() -> anyhow::Result<()> {
        let t = TempDir::new()?;

        unix::fs::symlink("x", t.path().join("y"))?;

        assert_eq!(
            None,
            ExternalSymlink::from_disk(ForwardRelativePath::new("y")?, AbsPath::new(t.path())?)
        );

        Ok(())
    }
}
