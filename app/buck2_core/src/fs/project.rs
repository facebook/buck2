/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::ffi::OsStr;
use std::fs::File;
use std::path::Component;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Context;
use dupe::Dupe;
use ref_cast::RefCast;
use relative_path::RelativePathBuf;

use crate::fs::fs_util;
use crate::fs::paths::abs_norm_path::AbsNormPath;
use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::RelativePath;

#[derive(Debug, buck2_error::Error)]
enum ProjectRootError {
    #[error("Provided project root `{0}` is not equal to the canonicalized path `{1}`")]
    NotCanonical(AbsNormPathBuf, AbsNormPathBuf),
    #[error("Project root `{0}` not found in path `{1}`")]
    ProjectRootNotFound(ProjectRoot, AbsPathBuf),
}

/// The 'ProjectFilesystem' that contains the root path and the current working
/// directory (cwd). The root path is the project root as defined in this
/// library. The cwd will be the directory from which the command was invoked,
/// which is within the project root and hence relativized against it.
#[derive(Clone, Debug, Dupe, PartialEq, derive_more::Display, Allocative)]
#[display(fmt = "{root}")]
pub struct ProjectRoot {
    root: Arc<AbsNormPathBuf>,
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
        let path = fs_util::canonicalize(temp.path())?;
        let path = ProjectRoot::new(path)?;
        Ok(Self { path, _temp: temp })
    }

    pub fn path(&self) -> &ProjectRoot {
        &self.path
    }

    pub fn write_file(&self, path: &str, content: &str) {
        let path = ProjectRelativePath::new(path).unwrap();
        self.path().write_file(path, content, false).unwrap();
    }
}

impl ProjectRoot {
    pub fn new(root: AbsNormPathBuf) -> anyhow::Result<Self> {
        let canon = fs_util::canonicalize(&root).context("canonicalize project root")?;
        if canon != root {
            return Err(ProjectRootError::NotCanonical(root, canon).into());
        }
        Ok(ProjectRoot {
            // We store the canonicalized path here because even if path
            // is equal to the canonicalized path, it may differ in the slashes or the case.
            root: Arc::new(canon),
        })
    }

    pub fn new_unchecked(root: AbsNormPathBuf) -> ProjectRoot {
        ProjectRoot {
            root: Arc::new(root),
        }
    }

    pub fn root(&self) -> &AbsNormPath {
        &self.root
    }

    pub fn name(&self) -> Option<&str> {
        self.root.file_name().and_then(OsStr::to_str)
    }

    ///
    /// Takes a 'ProjectRelativePath' and resolves it against the current
    /// `project root`, yielding a 'AbsPathBuf'
    ///
    /// ```
    /// use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    /// use buck2_core::fs::project::ProjectRoot;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// if cfg!(not(windows)) {
    ///     let root = AbsNormPathBuf::from("/usr/local/fbsource/".into())?;
    ///     let fs = ProjectRoot::new_unchecked(root);
    ///
    ///     assert_eq!(
    ///         AbsNormPathBuf::from("/usr/local/fbsource/buck/BUCK".into())?,
    ///         fs.resolve(ProjectRelativePath::new("buck/BUCK")?)
    ///     );
    /// } else {
    ///     let root = AbsNormPathBuf::from("c:/open/fbsource/".into())?;
    ///     let fs = ProjectRoot::new_unchecked(root);
    ///
    ///     assert_eq!(
    ///         AbsNormPathBuf::from("c:/open/fbsource/buck/BUCK".into())?,
    ///         fs.resolve(ProjectRelativePath::new("buck/BUCK")?)
    ///     );
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve(&self, path: impl PathLike) -> AbsNormPathBuf {
        path.resolve(self).into_owned()
    }

    ///
    /// Takes a 'ProjectRelativePath' and converts it to a 'Path' that is relative to the project root.
    ///
    /// ```
    /// use std::path::PathBuf;
    ///
    /// use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    /// use buck2_core::fs::project::ProjectRoot;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// let root = if cfg!(not(windows)) {
    ///     AbsNormPathBuf::from("/usr/local/fbsource/".into())?
    /// } else {
    ///     AbsNormPathBuf::from("c:/open/fbsource/".into())?
    /// };
    /// let fs = ProjectRoot::new_unchecked(root);
    ///
    /// assert_eq!(
    ///     PathBuf::from("buck/BUCK"),
    ///     fs.as_relative_path(ProjectRelativePath::new("buck/BUCK")?)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn as_relative_path<P: AsRef<ProjectRelativePath>>(&self, path: P) -> PathBuf {
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
    ///
    /// use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
    /// use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
    /// use buck2_core::fs::project::ProjectRoot;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// if cfg!(not(windows)) {
    ///     let root = AbsNormPathBuf::from("/usr/local/fbsource/".into())?;
    ///     let fs = ProjectRoot::new_unchecked(root);
    ///
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsNormPath::new("/usr/local/fbsource/src/buck.java")?)?
    ///     );
    ///     assert!(fs.relativize(AbsNormPath::new("/other/path")?).is_err());
    /// } else {
    ///     let root = AbsNormPathBuf::from("c:/open/fbsource/".into())?;
    ///     let fs = ProjectRoot::new_unchecked(root);
    ///
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsNormPath::new("c:/open/fbsource/src/buck.java")?)?
    ///     );
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsNormPath::new(r"C:\open\fbsource\src\buck.java")?)?
    ///     );
    ///     assert_eq!(
    ///         Cow::Borrowed(ProjectRelativePath::new("src/buck.java")?),
    ///         fs.relativize(AbsNormPath::new(r"\\?\C:\open\fbsource\src\buck.java")?)?
    ///     );
    ///     assert!(fs.relativize(AbsNormPath::new("c:/other/path")?).is_err());
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn relativize<'a, P: ?Sized + AsRef<AbsNormPath>>(
        &self,
        p: &'a P,
    ) -> anyhow::Result<Cow<'a, ProjectRelativePath>> {
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

    /// Remove project root prefix from path (even if path is not canonical)
    /// and return the remaining path.
    ///
    /// Fail if canonicalized path does not start with project root.
    fn strip_project_root<'a>(&'a self, path: &'a AbsPath) -> anyhow::Result<PathBuf> {
        let path = fs_util::simplified(path)?;

        if let Ok(rem) = Path::strip_prefix(path, &*self.root) {
            // Fast code path.
            return Ok(rem.to_path_buf());
        }

        // Now try to canonicalize the path. We cannot call `canonicalize` on the full path
        // because we should only resolve symlinks found in the past that point into the project,
        // but
        // * not symlink found inside the project that point outside of it
        // * not even symlinks found in the project unless we need to to resolve ".."

        let mut current_prefix = PathBuf::new();

        let mut components = path.components();
        while let Some(comp) = components.next() {
            current_prefix.push(comp);

            // This is not very efficient, but efficient cross-platform implementation is not easy.
            let canonicalized_current_prefix = fs_util::canonicalize(&current_prefix)?;

            if let Ok(rem) = canonicalized_current_prefix
                .as_path()
                .strip_prefix(self.root.as_path())
            {
                // We found the project root.
                return Ok(rem.join(components.as_path()));
            }
        }

        Err(ProjectRootError::ProjectRootNotFound(self.dupe(), path.to_owned()).into())
    }

    fn relativize_any_impl(&self, path: &AbsPath) -> anyhow::Result<ProjectRelativePathBuf> {
        let project_relative = self.strip_project_root(path)?;
        // TODO(nga): this does not treat `..` correctly.
        //   See the test below for an example.
        // This must use `RelativePathBuf`, not `RelativePath`,
        // because `RelativePathBuf` handles backslashes on Windows, and `RelativePath` does not.
        ProjectRelativePath::empty().join_normalized(RelativePathBuf::from_path(project_relative)?)
    }

    /// Relativize an absolute path which may be not normalized or not canonicalize.
    /// This operation may involve disk access.
    pub fn relativize_any<P: AsRef<AbsPath>>(
        &self,
        path: P,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        let path = path.as_ref();
        self.relativize_any_impl(path.as_ref()).with_context(|| {
            format!(
                "relativize path `{}` against project root `{}`",
                path.display(),
                self
            )
        })
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
    pub fn set_executable(&self, path: impl PathLike) -> anyhow::Result<()> {
        let path = path.resolve(self);
        fs_util::set_executable(path.as_ref())
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
                "Error copying from src path `{}` to dest path `{}`",
                src_abs, dest_abs
            )
        })
    }

    fn copy_resolved(
        &self,
        src_abs: &AbsNormPathBuf,
        dest_abs: &AbsNormPathBuf,
    ) -> anyhow::Result<()> {
        let src_type = fs_util::symlink_metadata(src_abs)?.file_type();

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

        // TODO: This should probably not use symlink_metadata_if_available... But it used to use
        // Path::exists()...
        let meta = match fs_util::symlink_metadata_if_available(&path) {
            Some(m) => m,
            None => return Ok(()),
        };

        let path_type = meta.file_type();

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
    fn find_relative_path(target: &AbsNormPathBuf, dest: &AbsNormPathBuf) -> PathBuf {
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
    fn copy_symlink(src: &AbsNormPathBuf, dest: &AbsNormPathBuf) -> anyhow::Result<()> {
        let mut target = fs_util::read_link(src)?;
        if target.is_relative() {
            // Grab the absolute path, then re-relativize the path to the destination
            let relative_target = fs_util::relative_path_from_system(target.as_path())?;
            let absolute_target = relative_target.normalize().to_path(
                src.parent()
                    .expect("a path with a parent in symlink target"),
            );
            target = Self::find_relative_path(&AbsNormPathBuf::try_from(absolute_target)?, dest);
        }
        fs_util::symlink(target, dest)
    }

    fn copy_file(src: &AbsNormPathBuf, dst: &AbsNormPathBuf) -> anyhow::Result<()> {
        fs_util::copy(src, dst).map(|_| ())
    }

    fn copy_dir(src_dir: &AbsNormPathBuf, dest_dir: &AbsNormPathBuf) -> anyhow::Result<()> {
        fs_util::create_dir_all(dest_dir)?;
        for file in fs_util::read_dir(src_dir)? {
            let file = file?;
            let filetype = file.file_type()?;
            let src_file = file.path();
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

use allocative::Allocative;
pub use internals::PathLike;

use crate::fs::paths::abs_path::AbsPath;
use crate::fs::paths::abs_path::AbsPathBuf;
use crate::fs::project_rel_path::ProjectRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;

mod internals {
    use std::borrow::Cow;

    use crate::fs::paths::abs_norm_path::AbsNormPath;
    use crate::fs::paths::abs_norm_path::AbsNormPathBuf;
    use crate::fs::project::ProjectRoot;
    use crate::fs::project_rel_path::ProjectRelativePath;
    use crate::fs::project_rel_path::ProjectRelativePathBuf;

    pub trait PathLike: PathLikeResolvable {}

    impl<T> PathLike for T where T: PathLikeResolvable {}

    pub trait PathLikeResolvable {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsNormPath>;
    }

    impl PathLikeResolvable for &AbsNormPath {
        fn resolve(&self, _fs: &ProjectRoot) -> Cow<'_, AbsNormPath> {
            Cow::Borrowed(self)
        }
    }

    impl PathLikeResolvable for &AbsNormPathBuf {
        fn resolve(&self, _fs: &ProjectRoot) -> Cow<'_, AbsNormPath> {
            Cow::Borrowed(self)
        }
    }

    impl PathLikeResolvable for &ProjectRelativePath {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsNormPath> {
            Cow::Owned(self.0.resolve(fs.root()))
        }
    }

    impl PathLikeResolvable for &ProjectRelativePathBuf {
        fn resolve(&self, fs: &ProjectRoot) -> Cow<'_, AbsNormPath> {
            Cow::Owned(self.0.resolve(fs.root()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::path::PathBuf;

    use crate::fs::fs_util;
    use crate::fs::paths::abs_path::AbsPath;
    use crate::fs::paths::forward_rel_path::ForwardRelativePath;
    use crate::fs::project::ProjectRoot;
    use crate::fs::project::ProjectRootTemp;
    use crate::fs::project_rel_path::ProjectRelativePath;

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

        fs_util::create_dir_all(fs.path.resolve(dir1))?;
        fs_util::create_dir_all(fs.path.resolve(dir2))?;
        fs_util::create_dir_all(fs.path.resolve(dir3))?;
        fs_util::create_dir_all(fs.path.resolve(out_dir))?;

        fs_util::write(fs.path.resolve(file1), "file1 contents")?;
        fs_util::write(fs.path.resolve(file2), "file2 contents")?;
        fs_util::write(fs.path.resolve(file3), "file3 contents")?;
        fs_util::write(fs.path.resolve(file4), "file4 contents")?;
        // Absolute path
        fs_util::symlink(fs.path.resolve(dir2), fs.path.resolve(link_dir2))?;
        // Relative path
        fs_util::symlink(Path::new("dir2/dir3"), fs.path.resolve(link_dir3))?;
        fs_util::symlink(Path::new("dir2/dir3/file3"), fs.path.resolve(link_file3))?;

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

        assert!(std::path::Path::is_dir(
            fs.path.resolve(expected_dir1).as_ref()
        ));
        assert!(std::path::Path::is_dir(
            fs.path.resolve(expected_dir2).as_ref()
        ));
        assert!(std::path::Path::is_dir(
            fs.path.resolve(expected_dir3).as_ref()
        ));
        // Absolute link path
        assert_eq!(
            fs.path.resolve(dir2).as_ref() as &Path,
            fs_util::read_link(fs.path.resolve(expected_link_dir2))?.as_path(),
        );
        // Make sure out/dir1/link_dir3 links to the relative path to dir1/dir2/dir3
        let link_dir3_target = fs_util::read_link(fs.path.resolve(expected_link_dir3))?;
        if cfg!(unix) {
            assert_eq!(
                Path::new("../../dir1/dir2/dir3"),
                link_dir3_target.as_path(),
            );
        } else {
            // In Windows we use absolute path
            assert_eq!(fs.path.resolve(dir3).as_path(), link_dir3_target.as_path());
        }

        // Make sure we can read through; that the relative path actually works
        fs_util::write(fs.path.resolve(file3), "file3 new contents")?;
        let link_file3_target = fs_util::read_link(fs.path.resolve(expected_link_file3))?;
        if cfg!(unix) {
            assert_eq!(
                Path::new("../../dir1/dir2/dir3/file3"),
                link_file3_target.as_path(),
            );
        } else {
            // In Windows we use absolute path
            assert_eq!(
                fs.path.resolve(file3).as_path(),
                link_file3_target.as_path()
            );
        }
        assert_eq!(
            "file3 new contents",
            fs_util::read_to_string(fs.path.resolve(expected_link_file3))?
        );

        assert_eq!(
            "file1 contents",
            fs_util::read_to_string(fs.path.resolve(expected_file1))?
        );
        assert_eq!(
            "file2 contents",
            fs_util::read_to_string(fs.path.resolve(expected_file2))?
        );
        // Independent copy; no hard links made (previous behavior)
        assert_eq!(
            "file3 contents",
            fs_util::read_to_string(fs.path.resolve(expected_file3))?
        );
        assert_eq!(
            "file4 contents",
            fs_util::read_to_string(fs.path.resolve(expected_file4))?
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

        assert_eq!("hello", fs_util::read_to_string(fs.path.resolve(symlink1))?);
        assert_eq!("hello", fs_util::read_to_string(fs.path.resolve(symlink2))?);
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

        let contents1 = fs_util::read_to_string(fs.path.resolve(dest1))?;
        let contents2 = fs_util::read_to_string(fs.path.resolve(dest2))?;
        let contents3 = fs_util::read_to_string(fs.path.resolve(dest3))?;
        let contents4 = fs_util::read_to_string(
            fs.path
                .resolve(dest4)
                .join(ForwardRelativePath::new("file")?),
        )?;
        let contents5 = fs_util::read_to_string(
            fs.path
                .resolve(dest5)
                .join(ForwardRelativePath::new("file")?),
        )?;

        if cfg!(unix) {
            assert_eq!(dest1_expected, dest1_value);
            assert_eq!(dest2_expected, dest2_value);
            assert_eq!(dest3_expected, dest3_value);
            assert_eq!(dest4_expected, dest4_value);
            assert_eq!(dest5_expected, dest5_value);
        } else {
            // In Windows we use absolute path
            assert_eq!(fs.path.resolve(target1).as_path(), dest1_value);
            assert_eq!(fs.path.resolve(target1).as_path(), dest2_value);
            assert_eq!(fs.path.resolve(target1).as_path(), dest3_value);
            assert_eq!(fs.path.resolve(target2).as_path(), dest4_value);
            assert_eq!(fs.path.resolve(target2).as_path(), dest5_value);
        }

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

        let content = fs_util::read_to_string(fs.path.resolve(dest_file))?;
        let new_content = fs_util::read_to_string(fs.path.resolve(new_file2))?;

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

    #[cfg(unix)]
    #[test]
    fn test_set_executable() -> anyhow::Result<()> {
        use std::os::unix::fs::PermissionsExt;

        let fs = ProjectRootTemp::new()?;

        // We can delete a read-only file
        let file = ProjectRelativePath::new("foo/bar/file")?;
        let real_file = fs.path.resolve(file);

        fs.path.write_file(file, "Hello", false)?;
        let perm = fs_util::metadata(&real_file)?.permissions();
        assert_eq!(perm.mode() & 0o111, 0);

        fs.path.set_executable(file)?;
        let perm = fs_util::metadata(&real_file)?.permissions();
        assert_eq!(perm.mode() & 0o111, 0o111);

        Ok(())
    }

    #[test]
    fn test_strip_project_root_simple() {
        let project_root = ProjectRootTemp::new().unwrap();
        assert_eq!(
            Path::new(""),
            project_root
                .path()
                .strip_project_root(project_root.path.root().as_abs_path())
                .unwrap()
        );
        assert_eq!(
            Path::new("foo"),
            project_root
                .path()
                .strip_project_root(&project_root.path.root().as_abs_path().join("foo"))
                .unwrap()
        );
        assert_eq!(
            Path::new("foo/bar"),
            project_root
                .path()
                .strip_project_root(&project_root.path.root().as_abs_path().join("foo/bar"))
                .unwrap()
        );
    }

    #[test]
    fn test_strip_project_root_complex() {
        if cfg!(windows) {
            return;
        }

        let project_root = ProjectRootTemp::new().unwrap();
        let temp_dir = tempfile::tempdir().unwrap();
        let temp_dir = AbsPath::new(temp_dir.path()).unwrap();

        fs_util::symlink(
            project_root.path.root(),
            AbsPath::new(&temp_dir.join("foo")).unwrap(),
        )
        .unwrap();
        assert_eq!(
            Path::new(""),
            project_root
                .path()
                .strip_project_root(&temp_dir.join("foo"))
                .unwrap()
        );
        assert_eq!(
            Path::new("bar"),
            project_root
                .path()
                .strip_project_root(&temp_dir.join("foo/bar"))
                .unwrap()
        );
    }

    #[test]
    fn test_relativize_any_bug() {
        if cfg!(windows) {
            return;
        }

        let project_root = ProjectRootTemp::new().unwrap();
        let project_root = project_root.path();

        fs_util::create_dir(project_root.root().as_abs_path().join("foo")).unwrap();
        fs_util::create_dir(project_root.root().as_abs_path().join("foo/bar")).unwrap();
        fs_util::create_dir(project_root.root().as_abs_path().join("link-target")).unwrap();
        fs_util::write(
            project_root.root().as_abs_path().join("link-target/fff"),
            "hello",
        )
        .unwrap();
        fs_util::symlink(
            Path::new("../../link-target"),
            project_root
                .root()
                .join(ForwardRelativePath::new("foo/bar/baz").unwrap()),
        )
        .unwrap();

        // Now explaining why the assertion in the end of the test is incorrect:
        // Existing path is resolved to non-existing path.

        let existing_path = "foo/bar/baz/../link-target/fff";
        let non_exist_path = "foo/bar/link-target/fff";
        assert!(
            fs_util::try_exists(project_root.root().as_abs_path().join(existing_path)).unwrap()
        );
        assert!(
            !fs_util::try_exists(project_root.root().as_abs_path().join(non_exist_path)).unwrap()
        );

        assert_eq!(
            ProjectRelativePath::new(non_exist_path).unwrap(),
            project_root
                .relativize_any(
                    project_root
                        .root()
                        .as_abs_path()
                        .join(Path::new(existing_path))
                )
                .unwrap()
        );
    }
}
