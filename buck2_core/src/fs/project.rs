/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! 'ProjectRelativePath's are normalized, platform agnostic, forward pointing
//! relative paths based at the `project root`.
//! The `project root` is an 'AbsPath' that corresponds to the root of the buck
//! process. This is not the current directory where the buck process is
//! invoked. It is the path of the root of the buck project, which defines the
//! buck version and configurations.
//!
//! The 'ProjectFilesystem' is the filesystem containing the `project root`
//! information. This file system is used to interact with the
//! 'ProjectRelativePath', and resolve the paths into a [`std::path::Path`] to
//! perform IO.
//!
//! Sample uses
//! ```
//! use buck2_core::fs::project::{ProjectRoot, ProjectRelativePathBuf, ProjectRelativePath};
//! use buck2_core::fs::paths::{AbsPathBuf, AbsPath, ForwardRelativePath};
//! use relative_path::RelativePath;
//! use std::{borrow::Cow, convert::TryFrom};
//!
//! let root = if cfg!(not(windows)) {
//!     AbsPathBuf::from("/usr/local/fbsource/".into())?
//! } else {
//!     AbsPathBuf::from("C:\\open\\fbsource\\".into())?
//! };
//! let some_path = if cfg!(not(windows)) {
//!     AbsPath::new("/usr/local/fbsource/buck/BUCK")?
//! } else {
//!     AbsPath::new("c:/open/fbsource/buck/BUCK")?
//! };
//!
//! let fs = ProjectRoot::new(root);
//! let project_rel = fs.relativize(some_path)?;
//!
//! assert_eq!(Cow::Borrowed(ProjectRelativePath::new("buck/BUCK")?), project_rel);
//! assert_eq!(some_path.to_buf(), fs.resolve(project_rel.as_ref()));
//!
//! let rel_path = RelativePath::new("../src");
//! let project_rel_2 = project_rel.join_normalized(rel_path)?;
//! assert_eq!(ProjectRelativePathBuf::try_from("buck/src".to_owned())?, project_rel_2);
//!
//! assert_eq!(some_path.join_normalized(rel_path)?, fs.resolve(&project_rel_2).to_buf());
//!
//! # anyhow::Ok(())
//! ```
//!
use std::borrow::Borrow;
use std::borrow::Cow;
use std::fs::File;
use std::ops::Deref;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use derivative::Derivative;
use derive_more::Display;
use gazebo::dupe::Dupe;
use ref_cast::RefCast;
use serde::Serialize;

use crate::fs::fs_util;
use crate::fs::paths::fmt::quoted_display;
use crate::fs::paths::AbsPath;
use crate::fs::paths::AbsPathBuf;
use crate::fs::paths::ForwardRelativePath;
use crate::fs::paths::ForwardRelativePathBuf;
use crate::fs::paths::RelativePath;
use crate::fs::paths::RelativePathBuf;

/// A un-owned forward pointing, fully normalized path that is relative to the
/// project root.
#[derive(Display, Derivative, Hash, PartialEq, Eq, PartialOrd, Ord, RefCast)]
#[derivative(Debug)]
#[repr(transparent)]
pub struct ProjectRelativePath(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePath,
);

/// The owned version of the 'ProjectRelativePath'
#[derive(Clone, Display, Derivative)]
// split in two because formatters don't agree
#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[derivative(Debug)]
pub struct ProjectRelativePathBuf(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePathBuf,
);

/// The 'ProjectFilesystem' that contains the root path and the current working
/// directory (cwd). The root path is the project root as defined in this
/// library. The cwd will be the directory from which the command was invoked,
/// which is within the project root and hence relativized against it.
#[derive(Clone, Debug, Dupe, PartialEq, derive_more::Display)]
#[display(fmt = "{root}")]
pub struct ProjectRoot {
    root: Arc<AbsPathBuf>,
}

pub struct ProjectRootTemp {
    path: ProjectRoot,
    // Important field as we want to keep this alive while the path is in use
    _temp: tempfile::TempDir,
}

impl ProjectRootTemp {
    /// creates a filesystem at a temporary root where the cwd is set to the
    /// same root
    pub fn new() -> anyhow::Result<Self> {
        let temp = tempfile::tempdir()?;
        let path = ProjectRoot::new(AbsPathBuf::try_from(temp.path().to_owned())?);
        Ok(Self { path, _temp: temp })
    }

    pub fn path(&self) -> &ProjectRoot {
        &self.path
    }
}

impl ProjectRoot {
    pub fn new(root: AbsPathBuf) -> Self {
        ProjectRoot {
            root: Arc::new(root),
        }
    }

    pub fn root(&self) -> &AbsPath {
        &*self.root
    }

    ///
    /// Takes a 'ProjectRelativePath' and resolves it against the current
    /// `project root`, yielding a 'AbsPathBuf'
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::{ProjectRoot, ProjectRelativePath};
    /// use buck2_core::fs::paths::AbsPathBuf;
    ///
    /// if cfg!(not(windows)) {
    ///     let root = AbsPathBuf::from("/usr/local/fbsource/".into())?;
    ///     let fs = ProjectRoot::new(root);
    ///
    ///     assert_eq!(
    ///         AbsPathBuf::from("/usr/local/fbsource/buck/BUCK".into())?,
    ///         fs.resolve(ProjectRelativePath::new("buck/BUCK")?)
    ///     );
    /// } else {
    ///     let root = AbsPathBuf::from("c:/open/fbsource/".into())?;
    ///     let fs = ProjectRoot::new(root);
    ///
    ///     assert_eq!(
    ///         AbsPathBuf::from("c:/open/fbsource/buck/BUCK".into())?,
    ///         fs.resolve(ProjectRelativePath::new("buck/BUCK")?)
    ///     );
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve(&self, path: impl PathLike) -> AbsPathBuf {
        path.resolve(self).into_owned()
    }

    ///
    /// Takes a 'ProjectRelativePath' and converts it to a 'Path' that is relative to the project root.
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::{ProjectRoot, ProjectRelativePath};
    /// use buck2_core::fs::paths::AbsPathBuf;
    /// use std::path::PathBuf;
    ///
    /// let root = if cfg!(not(windows)) {
    ///     AbsPathBuf::from("/usr/local/fbsource/".into())?
    /// } else {
    ///     AbsPathBuf::from("c:/open/fbsource/".into())?
    /// };
    /// let fs = ProjectRoot::new(root);
    ///
    /// assert_eq!(
    ///     PathBuf::from("buck/BUCK"),
    ///     fs.as_relative_path(ProjectRelativePath::new("buck/BUCK")?)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn as_relative_path<P: ?Sized + AsRef<ProjectRelativePath>>(&self, path: &P) -> PathBuf {
        let rel: &RelativePath = (path.as_ref().0).as_ref();
        PathBuf::from(rel.as_str())
    }

    ///
    /// Given an 'AbsPath', attempts to relativize the 'AbsPath' against the
    /// `project root` by stripping the prefix of the given paths.
    ///
    /// Errors if the given path is not a sub directory of the root.
    ///
    /// ```
    /// use std::borrow::Cow;
    /// use buck2_core::fs::project::{ProjectRoot, ProjectRelativePath};
    /// use buck2_core::fs::paths::{AbsPathBuf, AbsPath};
    ///
    /// if cfg!(not(windows)) {
    ///     let root = AbsPathBuf::from("/usr/local/fbsource/".into())?;
    ///     let fs = ProjectRoot::new(root);
    ///
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsPath::new("/usr/local/fbsource/src/buck.java")?)?
    ///     );
    ///     assert!(fs.relativize(AbsPath::new("/other/path")?).is_err());
    /// } else {
    ///     let root = AbsPathBuf::from("c:/open/fbsource/".into())?;
    ///     let fs = ProjectRoot::new(root);
    ///
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsPath::new("c:/open/fbsource/src/buck.java")?)?
    ///     );
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsPath::new(r"C:\open\fbsource\src\buck.java")?)?
    ///     );
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsPath::new(r"\\?\C:\open\fbsource\src\buck.java")?)?
    ///     );
    ///     assert!(fs.relativize(AbsPath::new("c:/other/path")?).is_err());
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn relativize<'a, P: ?Sized + AsRef<AbsPath>>(
        &'a self,
        p: &'a P,
    ) -> anyhow::Result<Cow<ProjectRelativePath>> {
        let relative_path = p.as_ref().strip_prefix(self.root()).map_err(|_| {
            anyhow::anyhow!(
                "Error relativizing: `{}` is not relative to project root `{}`",
                p.as_ref(),
                self.root()
            )
        })?;
        match relative_path {
            Cow::Borrowed(p) => Ok(Cow::Borrowed(ProjectRelativePath::ref_cast(p))),
            Cow::Owned(p) => Ok(Cow::Owned(ProjectRelativePathBuf::from(p))),
        }
    }

    // TODO(nga): refactor this to global function.
    pub fn write_file(
        &self,
        path: impl PathLike,
        contents: impl AsRef<[u8]>,
        executable: bool,
    ) -> anyhow::Result<()> {
        let abs_path = path.resolve(self);
        if let Some(parent) = abs_path.parent() {
            fs_util::create_dir_all(parent).with_context(|| {
                format!(
                    "`write_file` for `{}` creating directory `{}`",
                    abs_path.as_ref(),
                    parent
                )
            })?;
        }
        fs_util::write(abs_path.as_ref(), contents)
            .with_context(|| format!("`write_file` writing `{}`", abs_path.as_ref()))?;
        if executable {
            self.set_executable(abs_path.as_ref()).with_context(|| {
                format!("`write_file` setting executable `{}`", abs_path.as_ref())
            })?;
        }
        Ok(())
    }

    // TODO(nga): refactor this to global function.
    pub fn create_file(&self, path: impl PathLike, executable: bool) -> anyhow::Result<File> {
        let abs_path = path.resolve(self);
        if let Some(parent) = abs_path.parent() {
            fs_util::create_dir_all(parent).with_context(|| {
                format!(
                    "`create_file` for `{}` creating directory `{}`",
                    abs_path.as_ref(),
                    parent
                )
            })?;
        }
        let file = File::create(abs_path.as_ref())
            .with_context(|| format!("`create_file` creating `{}`", abs_path.as_ref()))?;
        if executable {
            self.set_executable(abs_path.as_ref()).with_context(|| {
                format!("`create_file` setting executable `{}`", abs_path.as_ref())
            })?;
        }
        Ok(file)
    }

    // TODO(nga): refactor this to global function.
    #[cfg(unix)]
    pub fn set_executable(&self, path: impl PathLike) -> anyhow::Result<()> {
        use std::os::unix::fs::PermissionsExt;
        // Unix permission bits
        let mut perms = fs_util::metadata(path.resolve(self).as_ref())?.permissions();
        // Add u+x
        perms.set_mode(perms.mode() | 0o100);
        fs_util::set_permissions(path.resolve(self).as_ref(), perms)?;
        Ok(())
    }

    // TODO(nga): refactor this to global function.
    #[cfg(not(unix))]
    pub fn set_executable(&self, _path: impl PathLike) -> anyhow::Result<()> {
        // Nothing to do
        Ok(())
    }

    /// Create a soft link from one location to another.
    ///
    /// There is no requirement that `src` must exist,
    /// and `src` can be either an absolute or relative path.
    ///
    /// This function is "raw" in the sense that it passes the `src`
    /// directly to underlying fs function calls. We do not verify
    /// anything about the incoming path. Other functions generally
    /// require things like "is a project relative path", etc.
    ///
    /// Filesystems that do not support soft links will return `Err`.
    // TODO(nga): refactor this to global function.
    pub fn soft_link_raw(&self, src: impl AsRef<Path>, dest: impl PathLike) -> anyhow::Result<()> {
        let dest_abs = self.resolve(dest);

        if let Some(parent) = dest_abs.parent() {
            fs_util::create_dir_all(parent)?;
        }
        fs_util::symlink(src, dest_abs)
    }

    /// Create a relative symlink between two relative paths
    ///
    /// This changes the path that `dest` is linked to, to the
    /// relative path to get to `src` from `dest`. Useful when
    /// one wants to link together, e.g. to `ProjectRelativePath`s
    ///
    /// e.g. given a `src` of `foo/bar1/baz1/out` and `dest` of `foo/bar2/baz2/out`,
    ///      `readlink` on `dest` would yield `../../bar1/baz1/out`
    ///
    /// `src`: Relative path that does not need to exist
    /// `dest`: Relative path that will be linked to `src`
    ///         using the relative traversal between the two
    ///
    /// Errors if the link could not be created (generally due to FS support of symlinks)
    // TODO(nga): refactor this to global function.
    pub fn soft_link_relativized(
        &self,
        src: impl PathLike,
        dest: impl PathLike,
    ) -> anyhow::Result<()> {
        let target_abs = self.resolve(src);
        let dest_abs = self.resolve(dest);

        let target_relative = Self::find_relative_path(&target_abs, &dest_abs);
        if let Some(parent) = dest_abs.parent() {
            fs_util::create_dir_all(parent)?;
        }
        fs_util::symlink(target_relative, dest_abs)
    }

    /// Copy from one path to another. This works for both files and directories.
    ///
    /// This copy works by:
    ///  - Copying directories recursively
    ///  - Re-writing relative symlinks. That is, a link to `foo/bar` might end up
    ///    as `../../../other/foo/bar` in the destination. Absolute symlinks are not changed.
    // TODO(nga): refactor this to global function.
    pub fn copy(&self, src: impl PathLike, dest: impl PathLike) -> anyhow::Result<()> {
        let src_abs = self.resolve(src);
        let dest_abs = self.resolve(dest);

        let result = self.copy_resolved(&src_abs, &dest_abs);
        result.with_context(|| {
            format!(
                "When copying from src path `{}` to dest path `{}`",
                src_abs, dest_abs
            )
        })
    }

    fn copy_resolved(&self, src_abs: &AbsPathBuf, dest_abs: &AbsPathBuf) -> anyhow::Result<()> {
        let src_type = fs_util::symlink_metadata(&src_abs)?.file_type();

        if let Some(parent) = dest_abs.parent() {
            fs_util::create_dir_all(parent)?;
        }
        if src_type.is_dir() {
            Self::copy_dir(src_abs, dest_abs)
        } else if src_type.is_file() {
            Self::copy_file(src_abs, dest_abs)
        } else if src_type.is_symlink() {
            Self::copy_symlink(src_abs, dest_abs)
        } else {
            // If we want to handle special files, we'll need to use special traits
            // https://doc.rust-lang.org/std/os/unix/fs/trait.FileTypeExt.html
            Err(anyhow::anyhow!(
                "Attempted to copy a path ({}) of an unknown type",
                src_abs
            ))
        }
    }

    /// Remove a path recursively, regardless of it being a file or a directory (all contents
    /// deleted).
    /// This does not follow symlinks, and only removes the link itself.
    // TODO(nga): refactor this to global function.
    pub fn remove_path_recursive(&self, path: impl PathLike) -> anyhow::Result<()> {
        let path = self.resolve(path);
        if !path.exists() {
            return Ok(());
        }
        let path_type = fs_util::metadata(&path)?.file_type();

        if path_type.is_dir() {
            fs_util::remove_dir_all(&path)
                .with_context(|| format!("remove_path_recursive({}) on directory", &path))?;
        } else if path_type.is_file() || path_type.is_symlink() {
            fs_util::remove_file(&path)
                .with_context(|| format!("remove_path_recursive({}) on file", &path))?;
        } else {
            // If we want to handle special files, we'll need to use special traits
            // https://doc.rust-lang.org/std/os/unix/fs/trait.FileTypeExt.html
            return Err(anyhow::anyhow!(
                "remove_path_recursive, attempted to delete a path ({}) of an unknown type",
                path
            ));
        }

        Ok(())
    }

    /// Find the relative path between two paths within the project
    pub fn relative_path(&self, target: impl PathLike, dest: impl PathLike) -> PathBuf {
        Self::find_relative_path(&self.resolve(target), &self.resolve(dest))
    }

    /// Find the relative path between two absolute ones
    ///
    /// Given two absolute paths, get the relative path from `dest` to `target`.
    ///
    /// e.g. given a `target` of `/foo/bar1/baz1/out` and `dest` of `/foo/bar2/baz2/out`, the
    ///      result would be `../../bar1/baz1/out`
    fn find_relative_path(target: &AbsPathBuf, dest: &AbsPathBuf) -> PathBuf {
        use itertools::EitherOrBoth::*;
        use itertools::Itertools;
        // Assemble both the '../' traversal, and the component that will come after that
        let mut upward_traversal = PathBuf::new();
        let mut relative_to_common_path = PathBuf::new();
        // So that /foo/bar/quz and /baz/bar/quz don't look like the same path
        // in the second component
        let mut diverged = false;

        for component in target
            .iter()
            .zip_longest(dest.parent().expect("a path with a parent in dest").iter())
        {
            match component {
                Both(t, d) => {
                    if diverged || t != d {
                        diverged = true;
                        upward_traversal.push(Component::ParentDir);
                        relative_to_common_path.push(t);
                    }
                }
                Left(t) => {
                    diverged = true;
                    relative_to_common_path.push(t)
                }
                Right(_) => {
                    diverged = true;
                    upward_traversal.push(Component::ParentDir)
                }
            }
        }
        upward_traversal.push(relative_to_common_path);
        upward_traversal
    }

    /// Creates symbolic link `dest` which points at the same location as symlink `src`.
    fn copy_symlink(src: &AbsPathBuf, dest: &AbsPathBuf) -> anyhow::Result<()> {
        let mut target = fs_util::read_link(&src)?;
        if target.is_relative() {
            // Grab the absolute path, then re-relativize the path to the destination
            let relative_target = if cfg!(windows) {
                Cow::Owned(RelativePathBuf::from_path(target.as_path())?)
            } else {
                Cow::Borrowed(RelativePath::from_path(target.as_path())?)
            };
            let absolute_target = relative_target.normalize().to_path(
                &src.parent()
                    .expect("a path with a parent in symlink target"),
            );
            target = Self::find_relative_path(&AbsPathBuf::try_from(absolute_target)?, dest);
        }
        fs_util::symlink(target, dest)
    }

    fn copy_file(src: &AbsPathBuf, dst: &AbsPathBuf) -> anyhow::Result<()> {
        fs_util::copy(src, dst).map(|_| ())
    }

    fn copy_dir(src_dir: &AbsPathBuf, dest_dir: &AbsPathBuf) -> anyhow::Result<()> {
        fs_util::create_dir_all(&dest_dir)?;
        for file in fs_util::read_dir(&src_dir)? {
            let file = file?;
            let filetype = file.file_type()?;
            let src_file = AbsPathBuf::try_from(file.path())?;
            let dest_file = dest_dir.join(ForwardRelativePath::new(&file.file_name())?);
            if filetype.is_dir() {
                Self::copy_dir(&src_file, &dest_file)?;
            } else if filetype.is_symlink() {
                Self::copy_symlink(&src_file, &dest_file)?;
            } else if filetype.is_file() {
                Self::copy_file(&src_file, &dest_file)?;
            }
        }
        Ok(())
    }
}

impl AsRef<ForwardRelativePath> for ProjectRelativePath {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for ProjectRelativePath {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePath> for ProjectRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for ProjectRelativePathBuf {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePathBuf> for ProjectRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePathBuf {
        &self.0
    }
}

impl ProjectRelativePath {
    pub fn unchecked_new<S: ?Sized + AsRef<str>>(s: &S) -> &Self {
        ProjectRelativePath::ref_cast(ForwardRelativePath::unchecked_new(s))
    }

    pub fn empty() -> &'static Self {
        ProjectRelativePath::unchecked_new("")
    }

    /// Creates an 'ProjectRelativePath' if the given string represents a
    /// forward, normalized relative path, otherwise error.
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use std::path::Path;
    ///
    /// assert!(ProjectRelativePath::new("foo/bar").is_ok());
    /// assert!(ProjectRelativePath::new("").is_ok());
    /// assert!(ProjectRelativePath::new("/abs/bar").is_err());
    /// assert!(ProjectRelativePath::new("normalize/./bar").is_err());
    /// assert!(ProjectRelativePath::new("normalize/../bar").is_err());
    ///
    /// assert!(ProjectRelativePath::new(Path::new("foo/bar")).is_ok());
    /// assert!(ProjectRelativePath::new(Path::new("")).is_ok());
    /// assert!(ProjectRelativePath::new(Path::new("/abs/bar")).is_err());
    /// assert!(ProjectRelativePath::new(Path::new("normalize/./bar")).is_err());
    /// assert!(ProjectRelativePath::new(Path::new("normalize/../bar")).is_err());
    /// ```
    pub fn new<P: ?Sized + AsRef<Path>>(p: &P) -> anyhow::Result<&ProjectRelativePath> {
        Ok(ProjectRelativePath::ref_cast(ForwardRelativePath::new(p)?))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_forward_relative_path(&self) -> &ForwardRelativePath {
        &self.0
    }

    /// Creates an owned 'ProjectRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::fs::project::{ProjectRelativePathBuf, ProjectRelativePath};
    ///
    /// let path = ProjectRelativePath::new("foo/bar")?;
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()), path.join(other));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf(self.0.join(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// assert_eq!(
    ///     Some(ProjectRelativePath::new("foo")?),
    ///     ProjectRelativePath::new("foo/bar")?.parent()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn parent(&self) -> Option<&ProjectRelativePath> {
        self.0.parent().map(ProjectRelativePath::ref_cast)
    }

    /// Returns the final component of the `ProjectRelativePath`, if there is
    /// one.
    ///
    /// If the path is a normal file, this is the file name. If it's the path of
    /// a directory, this is the directory name.
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// assert_eq!(Some(FileName::unchecked_new("bin")), ProjectRelativePath::new("usr/bin")?.file_name());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_name(&self) -> Option<&FileName> {
        self.0.file_name()
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`.
    ///
    /// Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// let path = ProjectRelativePath::new("test/haha/foo.txt")?;
    ///
    /// assert_eq!(
    ///     path.strip_prefix(ProjectRelativePath::new("test")?)?,
    ///     ForwardRelativePath::new("haha/foo.txt")?
    /// );
    /// assert_eq!(path.strip_prefix(ProjectRelativePath::new("asdf")?).is_err(), true);
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn strip_prefix<'a, P: ?Sized>(
        &'a self,
        base: &'a P,
    ) -> anyhow::Result<&'a ForwardRelativePath>
    where
        P: AsRef<ProjectRelativePath>,
    {
        self.0.strip_prefix(&base.as_ref().0)
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// let path = ProjectRelativePath::new("some/foo")?;
    ///
    /// assert!(path.starts_with(ProjectRelativePath::new("some")?));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn starts_with<P: AsRef<ProjectRelativePath>>(&self, base: P) -> bool {
        self.0.starts_with(&base.as_ref().0)
    }

    /// Determines whether `child` is a suffix of `self`.
    /// Only considers whole path components to match.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// let path = ProjectRelativePath::new("some/foo")?;
    ///
    /// assert!(path.ends_with(ForwardRelativePath::new("foo").unwrap()));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn ends_with<P: AsRef<ForwardRelativePath>>(&self, child: P) -> bool {
        self.0.ends_with(child.as_ref())
    }

    /// Extracts the stem (non-extension) portion of [`self.file_name`].
    ///
    /// The stem is:
    ///
    /// * [`None`], if there is no file name;
    /// * The entire file name if there is no embedded `.`;
    /// * The entire file name if the file name begins with `.` and has no other
    ///   `.`s within;
    /// * Otherwise, the portion of the file name before the final `.`
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// let path = ProjectRelativePath::new("foo.rs")?;
    ///
    /// assert_eq!(Some("foo"), path.file_stem());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_stem(&self) -> Option<&str> {
        self.0.file_stem()
    }

    /// Extracts the extension of [`self.file_name`], if possible.
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePath;
    ///
    /// assert_eq!(Some("rs"), ProjectRelativePath::new("hi/foo.rs")?.extension());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    /// Build an owned `ProjectRelativePathBuf`, joined with the given path and
    /// normalized.
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::{ProjectRelativePath, ProjectRelativePathBuf};
    /// use std::convert::TryFrom;
    ///
    /// assert_eq!(
    ///     ProjectRelativePath::new("foo/bar")?.join_normalized("../baz.txt")?,
    ///     ProjectRelativePathBuf::unchecked_new("foo/baz.txt".into()),
    /// );
    ///
    /// assert_eq!(
    ///     ProjectRelativePath::new("foo")?.join_normalized("../../baz.txt").is_err(),
    ///     true
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(
        &self,
        path: P,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        let inner = self.0.join_normalized(path)?;
        // TODO need verify?
        Ok(ProjectRelativePathBuf(inner))
    }

    /// Iterator over the components of this path
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// let p = ProjectRelativePath::new("foo/bar/baz")?;
    /// let mut it = p.iter();
    ///
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("foo"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("bar"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("baz"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     None
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn iter(&self) -> ForwardRelativePathIter {
        self.0.iter()
    }

    pub fn to_buf(&self) -> ProjectRelativePathBuf {
        self.to_owned()
    }
}

impl<'a> From<&'a ForwardRelativePath> for &'a ProjectRelativePath {
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use std::convert::From;
    ///
    /// let f = ForwardRelativePath::new("foo")?;
    ///
    /// assert_eq!(<&ProjectRelativePath>::from(f), ProjectRelativePath::new("foo")?);
    ///
    /// # anyhow::Ok(())
    /// ```
    fn from(p: &'a ForwardRelativePath) -> &'a ProjectRelativePath {
        ProjectRelativePath::ref_cast(p)
    }
}

impl ProjectRelativePathBuf {
    pub fn unchecked_new(s: String) -> Self {
        Self(ForwardRelativePathBuf::unchecked_new(s))
    }

    /// Creates a new 'ProjectRelativePathBuf' with a given capacity used to create the internal
    /// 'String'. See 'with_capacity' defined on 'ForwardRelativePathBuf'
    pub fn with_capacity(cap: usize) -> Self {
        Self(ForwardRelativePathBuf::with_capacity(cap))
    }

    /// Returns the capacity of the underlying 'ForwardRelativePathBuf'
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Invokes 'reserve' on the underlying 'ForwardRelativePathBuf'
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Invokes 'shrink_to_fit' on the underlying 'ForwardRelativePathBuf'
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    /// Invokes 'shrink_to' on the underlying 'String'
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.0.shrink_to(min_capacity)
    }

    /// Pushes a `ForwardRelativePath` to the existing buffer
    pub fn push<P: AsRef<ForwardRelativePath>>(&mut self, path: P) {
        self.0.push(path)
    }

    /// Pushes a `RelativePath` to the existing buffer, normalizing it
    pub fn push_normalized<P: AsRef<RelativePath>>(&mut self, path: P) -> anyhow::Result<()> {
        self.0.push_normalized(path)
    }

    pub fn into_forward_relative_path_buf(self) -> ForwardRelativePathBuf {
        self.0
    }
}

impl From<ForwardRelativePathBuf> for ProjectRelativePathBuf {
    fn from(p: ForwardRelativePathBuf) -> Self {
        Self(p)
    }
}

impl From<ProjectRelativePathBuf> for ForwardRelativePathBuf {
    fn from(p: ProjectRelativePathBuf) -> Self {
        p.0
    }
}

impl From<ProjectRelativePathBuf> for RelativePathBuf {
    fn from(p: ProjectRelativePathBuf) -> Self {
        p.0.into()
    }
}

impl<'a> TryFrom<&'a str> for &'a ProjectRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    ///
    /// assert!(<&ProjectRelativePath>::try_from("foo/bar").is_ok());
    /// assert!(<&ProjectRelativePath>::try_from("").is_ok());
    /// assert!(<&ProjectRelativePath>::try_from("/abs/bar").is_err());
    /// assert!(<&ProjectRelativePath>::try_from("normalize/./bar").is_err());
    /// assert!(<&ProjectRelativePath>::try_from("normalize/../bar").is_err());
    /// ```
    fn try_from(s: &'a str) -> anyhow::Result<&'a ProjectRelativePath> {
        Ok(ProjectRelativePath::ref_cast(ForwardRelativePath::new(s)?))
    }
}

impl<'a> TryFrom<&'a RelativePath> for &'a ProjectRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::RelativePath;
    ///
    /// assert!(<&ProjectRelativePath>::try_from(RelativePath::new("foo/bar")).is_ok());
    /// assert!(<&ProjectRelativePath>::try_from(RelativePath::new("")).is_ok());
    /// assert!(<&ProjectRelativePath>::try_from(RelativePath::new("normalize/./bar")).is_err());
    /// assert!(<&ProjectRelativePath>::try_from(RelativePath::new("normalize/../bar")).is_err());
    /// ```
    fn try_from(s: &'a RelativePath) -> anyhow::Result<&'a ProjectRelativePath> {
        Ok(ProjectRelativePath::ref_cast(ForwardRelativePath::new(
            s.as_str(),
        )?))
    }
}

impl TryFrom<String> for ProjectRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(ProjectRelativePathBuf::try_from("foo/bar".to_owned()).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from("".to_owned()).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from("/abs/bar".to_owned()).is_err());
    /// assert!(ProjectRelativePathBuf::try_from("normalize/./bar".to_owned()).is_err());
    /// assert!(ProjectRelativePathBuf::try_from("normalize/../bar".to_owned()).is_err());
    /// ```
    fn try_from(s: String) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(ProjectRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(s)?,
        ))
    }
}

impl TryFrom<RelativePathBuf> for ProjectRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion (TODO make ForwardRelativePath a no allocation
    /// conversion)
    ///
    /// ```
    /// use buck2_core::fs::project::ProjectRelativePathBuf;
    /// use buck2_core::fs::paths::RelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(ProjectRelativePathBuf::try_from(RelativePathBuf::from("foo/bar")).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from(RelativePathBuf::from("")).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from(RelativePathBuf::from("normalize/./bar")).is_err());
    /// assert!(ProjectRelativePathBuf::try_from(RelativePathBuf::from("normalize/../bar")).is_err());
    /// ```
    fn try_from(p: RelativePathBuf) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(ProjectRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(p)?,
        ))
    }
}

impl TryFrom<PathBuf> for ProjectRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::project::ProjectRelativePathBuf;
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// assert!(ProjectRelativePathBuf::try_from(PathBuf::from("foo/bar")).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from(PathBuf::from("")).is_ok());
    /// assert!(ProjectRelativePathBuf::try_from(PathBuf::from("/abs/bar")).is_err());
    /// assert!(ProjectRelativePathBuf::try_from(PathBuf::from("normalize/./bar")).is_err());
    /// assert!(ProjectRelativePathBuf::try_from(PathBuf::from("normalize/../bar")).is_err());
    /// ```
    fn try_from(p: PathBuf) -> anyhow::Result<ProjectRelativePathBuf> {
        Ok(ProjectRelativePathBuf(ForwardRelativePathBuf::try_from(p)?))
    }
}

impl ToOwned for ProjectRelativePath {
    type Owned = ProjectRelativePathBuf;

    fn to_owned(&self) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf(self.0.to_owned())
    }
}

impl AsRef<ProjectRelativePath> for ProjectRelativePath {
    fn as_ref(&self) -> &ProjectRelativePath {
        self
    }
}

impl AsRef<ProjectRelativePath> for ProjectRelativePathBuf {
    fn as_ref(&self) -> &ProjectRelativePath {
        ProjectRelativePath::ref_cast(&self.0)
    }
}

impl Borrow<ProjectRelativePath> for ProjectRelativePathBuf {
    fn borrow(&self) -> &ProjectRelativePath {
        self.as_ref()
    }
}

impl Deref for ProjectRelativePathBuf {
    type Target = ProjectRelativePath;

    fn deref(&self) -> &ProjectRelativePath {
        ProjectRelativePath::ref_cast(&self.0)
    }
}

pub use internals::PathLike;

use crate::fs::paths::FileName;
use crate::fs::paths::ForwardRelativePathIter;

mod internals {
    use std::borrow::Cow;

    use crate::fs::paths::AbsPath;
    use crate::fs::paths::AbsPathBuf;
    use crate::fs::project::ProjectRelativePath;
    use crate::fs::project::ProjectRelativePathBuf;
    use crate::fs::project::ProjectRoot;

    pub trait PathLike: PathLikeResolvable {}

    impl<T> PathLike for T where T: PathLikeResolvable {}

    pub trait PathLikeResolvable {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsPath>;
    }

    impl PathLikeResolvable for &AbsPath {
        fn resolve(&self, _fs: &ProjectRoot) -> Cow<'_, AbsPath> {
            Cow::Borrowed(self)
        }
    }

    impl PathLikeResolvable for &AbsPathBuf {
        fn resolve(&self, _fs: &ProjectRoot) -> Cow<'_, AbsPath> {
            Cow::Borrowed(self)
        }
    }

    impl PathLikeResolvable for &ProjectRelativePath {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsPath> {
            Cow::Owned(self.0.resolve(fs.root()))
        }
    }

    impl PathLikeResolvable for &ProjectRelativePathBuf {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsPath> {
            Cow::Owned(self.0.resolve(fs.root()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::path::PathBuf;

    use crate::fs::fs_util;
    use crate::fs::paths::ForwardRelativePath;
    use crate::fs::project::ProjectRelativePath;
    use crate::fs::project::ProjectRelativePathBuf;
    use crate::fs::project::ProjectRoot;
    use crate::fs::project::ProjectRootTemp;

    #[test]
    fn path_display_is_readable() -> anyhow::Result<()> {
        let buf = ProjectRelativePathBuf::try_from("foo/bar".to_owned())?;
        assert_eq!("foo/bar", format!("{}", buf));
        assert_eq!("ProjectRelativePathBuf(\"foo/bar\")", format!("{:?}", buf));
        let refpath: &ProjectRelativePath = &buf;
        assert_eq!("foo/bar", format!("{}", refpath));
        assert_eq!("ProjectRelativePath(\"foo/bar\")", format!("{:?}", refpath));

        Ok(())
    }

    #[test]
    fn path_is_comparable() -> anyhow::Result<()> {
        let path1_buf = ProjectRelativePathBuf::try_from("foo".to_owned())?;
        let path2_buf = ProjectRelativePathBuf::try_from("foo".to_owned())?;
        let path3_buf = ProjectRelativePathBuf::try_from("bar".to_owned())?;

        let path1 = ProjectRelativePath::new("foo")?;
        let path2 = ProjectRelativePath::new("foo")?;
        let path3 = ProjectRelativePath::new("bar")?;

        let str2 = "foo";
        let str3 = "bar";
        let str_abs = "/ble";

        let string2 = "foo".to_owned();
        let string3 = "bar".to_owned();
        let string_abs = "/ble".to_owned();

        assert_eq!(path1_buf, path2_buf);
        assert_ne!(path1_buf, path3_buf);

        assert_eq!(path1, path2);
        assert_ne!(path1, path3);

        assert_eq!(path1_buf, path2);
        assert_ne!(path1, path3_buf);

        assert_eq!(path1_buf, str2);
        assert_ne!(path1_buf, str3);
        assert_ne!(path1_buf, str_abs);

        assert_eq!(path1, str2);
        assert_ne!(path1, str3);
        assert_ne!(path1, str_abs);

        assert_eq!(path1_buf, string2);
        assert_ne!(path1_buf, string3);
        assert_ne!(path1_buf, string_abs);

        assert_eq!(path1, string2);
        assert_ne!(path1, string3);
        assert_ne!(path1, string_abs);

        Ok(())
    }

    #[test]
    fn copy_works() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let dir1 = ProjectRelativePath::new("dir1")?;
        let dir2 = ProjectRelativePath::new("dir1/dir2")?;
        let dir3 = ProjectRelativePath::new("dir1/dir2/dir3")?;
        let link_dir2 = ProjectRelativePath::new("dir1/link_dir2")?;
        let link_dir3 = ProjectRelativePath::new("dir1/link_dir3")?;
        let link_file3 = ProjectRelativePath::new("dir1/link_file3")?;
        let file1 = ProjectRelativePath::new("dir1/file1")?;
        let file2 = ProjectRelativePath::new("dir1/dir2/file2")?;
        let file3 = ProjectRelativePath::new("dir1/dir2/dir3/file3")?;
        let file4 = ProjectRelativePath::new("dir1/dir2/dir3/file4")?;
        let out_dir = ProjectRelativePath::new("out")?;

        fs_util::create_dir_all(&fs.path.resolve(dir1))?;
        fs_util::create_dir_all(&fs.path.resolve(dir2))?;
        fs_util::create_dir_all(&fs.path.resolve(dir3))?;
        fs_util::create_dir_all(&fs.path.resolve(out_dir))?;

        fs_util::write(&fs.path.resolve(file1), "file1 contents")?;
        fs_util::write(&fs.path.resolve(file2), "file2 contents")?;
        fs_util::write(&fs.path.resolve(file3), "file3 contents")?;
        fs_util::write(&fs.path.resolve(file4), "file4 contents")?;
        // Absolute path
        fs_util::symlink(&fs.path.resolve(dir2), &fs.path.resolve(link_dir2))?;
        // Relative path
        fs_util::symlink(&Path::new("dir2/dir3"), &fs.path.resolve(link_dir3))?;
        fs_util::symlink(&Path::new("dir2/dir3/file3"), &fs.path.resolve(link_file3))?;

        fs.path
            .copy(
                ProjectRelativePath::new("dir1/file1")?,
                ProjectRelativePath::new("out")?,
            )
            .expect_err("should fail because out exists");

        let expected_dir1 = ProjectRelativePath::new("out/dir1")?;
        let expected_dir2 = ProjectRelativePath::new("out/dir1/dir2")?;
        let expected_dir3 = ProjectRelativePath::new("out/dir1/dir2/dir3")?;
        let expected_link_dir2 = ProjectRelativePath::new("out/dir1/link_dir2")?;
        let expected_link_dir3 = ProjectRelativePath::new("out/dir1/link_dir3")?;
        let expected_link_file3 = ProjectRelativePath::new("out/dir1/link_file3")?;
        let expected_file1 = ProjectRelativePath::new("out/dir1/file1")?;
        let expected_file2 = ProjectRelativePath::new("out/dir1/dir2/file2")?;
        let expected_file3 = ProjectRelativePath::new("out/dir1/dir2/dir3/file3")?;
        let expected_file4 = ProjectRelativePath::new("out/other1/file4")?;

        fs.path.copy(dir1, ProjectRelativePath::new("out/dir1")?)?;

        // Ensure copying a file creates any parent dirs properly
        fs.path
            .copy(file4, ProjectRelativePath::new("out/other1/file4")?)?;

        assert_eq!(
            true,
            std::path::Path::is_dir(fs.path.resolve(expected_dir1).as_ref())
        );
        assert_eq!(
            true,
            std::path::Path::is_dir(fs.path.resolve(expected_dir2).as_ref())
        );
        assert_eq!(
            true,
            std::path::Path::is_dir(fs.path.resolve(expected_dir3).as_ref())
        );
        // Absolute link path
        assert_eq!(
            fs.path.resolve(dir2).as_ref() as &Path,
            fs_util::read_link(fs.path.resolve(expected_link_dir2))?.as_path(),
        );
        // Make sure out/dir1/link_dir3 links to the relative path to dir1/dir2/dir3
        let link_dir3_target = fs_util::read_link(fs.path.resolve(expected_link_dir3))?;
        assert_eq!(
            Path::new("../../dir1/dir2/dir3"),
            link_dir3_target.as_path(),
        );

        // Make sure we can read through; that the relative path actually works
        fs_util::write(&fs.path.resolve(file3), "file3 new contents")?;
        let link_file3_target = fs_util::read_link(fs.path.resolve(expected_link_file3))?;
        assert_eq!(
            Path::new("../../dir1/dir2/dir3/file3"),
            link_file3_target.as_path(),
        );
        assert_eq!(
            "file3 new contents",
            fs_util::read_to_string(&fs.path.resolve(expected_link_file3))?
        );

        assert_eq!(
            "file1 contents",
            fs_util::read_to_string(&fs.path.resolve(expected_file1))?
        );
        assert_eq!(
            "file2 contents",
            fs_util::read_to_string(&fs.path.resolve(expected_file2))?
        );
        // Independent copy; no hard links made (previous behavior)
        assert_eq!(
            "file3 contents",
            fs_util::read_to_string(&fs.path.resolve(expected_file3))?
        );
        assert_eq!(
            "file4 contents",
            fs_util::read_to_string(&fs.path.resolve(expected_file4))?
        );
        Ok(())
    }

    #[test]
    fn test_copy_symlink() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let symlink1 = ProjectRelativePath::new("symlink1")?;
        let symlink2 = ProjectRelativePath::new("symlink2")?;
        let file = ProjectRelativePath::new("file")?;
        fs.path.write_file(file, "hello", false)?;
        fs.path.soft_link_raw(fs.path.resolve(file), symlink1)?;
        fs.path.copy(symlink1, symlink2)?;

        assert_eq!(
            "hello",
            fs_util::read_to_string(&fs.path.resolve(symlink1))?
        );
        assert_eq!(
            "hello",
            fs_util::read_to_string(&fs.path.resolve(symlink2))?
        );
        Ok(())
    }

    #[test]
    fn test_symlink_relativized() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;

        let target1 = ProjectRelativePath::new("foo1/bar1/target")?;
        let target2 = ProjectRelativePath::new("foo2/bar")?;
        let file = target2.join(ForwardRelativePath::new("file")?);

        let dest1 = ProjectRelativePath::new("foo1/target-link")?;
        let dest2 = ProjectRelativePath::new("foo1/bar2/target")?;
        let dest3 = ProjectRelativePath::new("foo1-link/bar1/target")?;
        let dest4 = ProjectRelativePath::new("foo2/bar-link")?;
        let dest5 = ProjectRelativePath::new("foo2-link/bar")?;

        fs.path.write_file(target1, "foo1 contents", false)?;
        fs.path.write_file(&file, "foo2 contents", false)?;

        fs.path.soft_link_relativized(target1, dest1)?;
        fs.path.soft_link_relativized(target1, dest2)?;
        fs.path.soft_link_relativized(target1, dest3)?;
        fs.path.soft_link_relativized(target2, dest4)?;
        fs.path.soft_link_relativized(target2, dest5)?;

        fs.path.write_file(target1, "new foo1 contents", false)?;
        fs.path.write_file(&file, "new foo2 contents", false)?;

        let dest1_expected = PathBuf::from("bar1/target");
        let dest2_expected = PathBuf::from("../bar1/target");
        let dest3_expected = PathBuf::from("../../foo1/bar1/target");
        let dest4_expected = PathBuf::from("bar");
        let dest5_expected = PathBuf::from("../foo2/bar");

        let dest1_value = fs_util::read_link(fs.path.resolve(dest1))?;
        let dest2_value = fs_util::read_link(fs.path.resolve(dest2))?;
        let dest3_value = fs_util::read_link(fs.path.resolve(dest3))?;
        let dest4_value = fs_util::read_link(fs.path.resolve(dest4))?;
        let dest5_value = fs_util::read_link(fs.path.resolve(dest5))?;

        let contents1 = fs_util::read_to_string(&fs.path.resolve(dest1))?;
        let contents2 = fs_util::read_to_string(&fs.path.resolve(dest2))?;
        let contents3 = fs_util::read_to_string(&fs.path.resolve(dest3))?;
        let contents4 = fs_util::read_to_string(
            &fs.path
                .resolve(dest4)
                .join(ForwardRelativePath::new("file")?),
        )?;
        let contents5 = fs_util::read_to_string(
            &fs.path
                .resolve(dest5)
                .join(ForwardRelativePath::new("file")?),
        )?;

        assert_eq!(dest1_expected, dest1_value);
        assert_eq!(dest2_expected, dest2_value);
        assert_eq!(dest3_expected, dest3_value);
        assert_eq!(dest4_expected, dest4_value);
        assert_eq!(dest5_expected, dest5_value);

        assert_eq!("new foo1 contents", contents1);
        assert_eq!("new foo1 contents", contents2);
        assert_eq!("new foo1 contents", contents3);
        assert_eq!("new foo2 contents", contents4);
        assert_eq!("new foo2 contents", contents5);

        Ok(())
    }

    #[test]
    fn test_symlink_to_directory() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;
        let source_dir = ProjectRelativePath::new("foo")?;
        let source_file = ProjectRelativePath::new("foo/file")?;
        let dest_dir = ProjectRelativePath::new("bar")?;
        let dest_file = ProjectRelativePath::new("bar/file")?;
        let new_file1 = ProjectRelativePath::new("bar/new_file")?;
        let new_file2 = ProjectRelativePath::new("foo/new_file")?;

        fs.path.write_file(source_file, "file content", false)?;
        fs.path.soft_link_relativized(source_dir, dest_dir)?;
        fs.path.write_file(new_file1, "new file content", false)?;

        let content = fs_util::read_to_string(&fs.path.resolve(dest_file))?;
        let new_content = fs_util::read_to_string(&fs.path.resolve(new_file2))?;

        assert_eq!("file content", content);
        assert_eq!("new file content", new_content);
        Ok(())
    }

    #[cfg(unix)]
    #[test]
    fn test_remove_readonly_path_recursive() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;

        // We can delete a read-only file
        let file = ProjectRelativePath::new("foo/bar/link")?;
        fs.path.write_file(file, "Hello", false)?;
        let real_file = fs.path.resolve(file);
        let mut perm = fs_util::metadata(&real_file)?.permissions();
        perm.set_readonly(true);
        fs_util::set_permissions(&real_file, perm)?;
        fs.path.remove_path_recursive(file)?;
        assert!(!fs.path.resolve(file).exists());
        Ok(())
    }

    #[test]
    fn test_relativizes_paths_correct() -> anyhow::Result<()> {
        let fs = ProjectRootTemp::new()?;

        let test_cases = vec![
            ("foo/bar/baz", "notfoo/bar/quz", "../../foo/bar/baz"),
            (
                "foo/bar/baz",
                "notfoo/some/deep/tree/out",
                "../../../../foo/bar/baz",
            ),
            (
                "notfoo/bar/quz",
                "notfoo/some/deep/tree/out",
                "../../../bar/quz",
            ),
            ("foo/bar", "foo/baz", "bar"),
            ("bar", "foo/baz", "../bar"),
            ("foo/bar", "baz", "foo/bar"),
        ];

        for (target_str, dest_str, expected_str) in test_cases {
            let expected = PathBuf::from(expected_str);
            let target = ProjectRelativePath::new(target_str)?;
            let dest = ProjectRelativePath::new(dest_str)?;

            let actual =
                ProjectRoot::find_relative_path(&fs.path.resolve(target), &fs.path.resolve(dest));
            assert_eq!(
                expected,
                actual,
                "Expected path from {} to {} to be {}, got {}",
                target_str,
                dest_str,
                expected_str,
                actual.as_path().to_string_lossy()
            );
        }

        Ok(())
    }
}
