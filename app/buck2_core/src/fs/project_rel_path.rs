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
//! use std::borrow::Cow;
//! use std::convert::TryFrom;
//!
//! use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
//! use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
//! use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
//! use buck2_core::fs::project::ProjectRoot;
//! use buck2_core::fs::project_rel_path::ProjectRelativePath;
//! use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
//! use relative_path::RelativePath;
//!
//! let root = if cfg!(not(windows)) {
//!     AbsNormPathBuf::from("/usr/local/fbsource/".into())?
//! } else {
//!     AbsNormPathBuf::from("C:\\open\\fbsource\\".into())?
//! };
//! let some_path = if cfg!(not(windows)) {
//!     AbsNormPath::new("/usr/local/fbsource/buck/BUCK")?
//! } else {
//!     AbsNormPath::new("c:/open/fbsource/buck/BUCK")?
//! };
//!
//! let fs = ProjectRoot::new_unchecked(root);
//! let project_rel = fs.relativize(some_path)?;
//!
//! assert_eq!(
//!     Cow::Borrowed(ProjectRelativePath::new("buck/BUCK")?),
//!     project_rel
//! );
//! assert_eq!(some_path.to_buf(), fs.resolve(project_rel.as_ref()));
//!
//! let rel_path = RelativePath::new("../src");
//! let project_rel_2 = project_rel.join_normalized(rel_path)?;
//! assert_eq!(
//!     ProjectRelativePathBuf::try_from("buck/src".to_owned())?,
//!     project_rel_2
//! );
//!
//! assert_eq!(
//!     some_path.join_normalized(rel_path)?,
//!     fs.resolve(&project_rel_2).to_buf()
//! );
//!
//! # anyhow::Ok(())
//! ```

use std::borrow::Borrow;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use allocative::Allocative;
use derivative::Derivative;
use ref_cast::RefCast;
use relative_path::RelativePath;
use relative_path::RelativePathBuf;
use serde::Deserialize;
use serde::Serialize;

use crate::fs::paths::file_name::FileName;
use crate::fs::paths::fmt::quoted_display;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePathIter;

/// A un-owned forward pointing, fully normalized path that is relative to the
/// project root.
#[derive(
    derive_more::Display,
    Derivative,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    RefCast
)]
#[derivative(Debug)]
#[repr(transparent)]
pub struct ProjectRelativePath(
    // TODO(nga): make private.
    #[derivative(Debug(format_with = "quoted_display"))] pub(crate) ForwardRelativePath,
);

/// The owned version of the 'ProjectRelativePath'
#[derive(Clone, derive_more::Display, Derivative)]
// split in two because formatters don't agree
#[derive(
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    Allocative
)]
#[derivative(Debug)]
pub struct ProjectRelativePathBuf(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePathBuf,
);

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
    /// use std::path::Path;
    ///
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates an owned 'ProjectRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    ///
    /// let path = ProjectRelativePath::new("foo/bar")?;
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(
    ///     ProjectRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()),
    ///     path.join(other)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf(self.0.join(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    /// use buck2_core::fs::paths::file_name::FileName;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// assert_eq!(
    ///     Some(FileName::unchecked_new("bin")),
    ///     ProjectRelativePath::new("usr/bin")?.file_name()
    /// );
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
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// let path = ProjectRelativePath::new("test/haha/foo.txt")?;
    ///
    /// assert_eq!(
    ///     path.strip_prefix(ProjectRelativePath::new("test")?)?,
    ///     ForwardRelativePath::new("haha/foo.txt")?
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(ProjectRelativePath::new("asdf")?)
    ///         .is_err(),
    ///     true
    /// );
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

    pub fn strip_prefix_opt<'a, P: ?Sized>(&'a self, base: &'a P) -> Option<&'a ForwardRelativePath>
    where
        P: AsRef<ProjectRelativePath>,
    {
        self.0.strip_prefix_opt(&base.as_ref().0)
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// ```
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// assert_eq!(
    ///     Some("rs"),
    ///     ProjectRelativePath::new("hi/foo.rs")?.extension()
    /// );
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
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
    ///
    /// assert_eq!(
    ///     ProjectRelativePath::new("foo/bar")?.join_normalized("../baz.txt")?,
    ///     ProjectRelativePathBuf::unchecked_new("foo/baz.txt".into()),
    /// );
    ///
    /// assert_eq!(
    ///     ProjectRelativePath::new("foo")?
    ///         .join_normalized("../../baz.txt")
    ///         .is_err(),
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
    /// use buck2_core::fs::paths::file_name::FileName;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// let p = ProjectRelativePath::new("foo/bar/baz")?;
    /// let mut it = p.iter();
    ///
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("foo")));
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("bar")));
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("baz")));
    /// assert_eq!(it.next(), None);
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
    /// use std::convert::From;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
    ///
    /// let f = ForwardRelativePath::new("foo")?;
    ///
    /// assert_eq!(
    ///     <&ProjectRelativePath>::from(f),
    ///     ProjectRelativePath::new("foo")?
    /// );
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

    pub fn testing_new(path: &str) -> ProjectRelativePathBuf {
        ProjectRelativePathBuf::try_from(path.to_owned()).unwrap()
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
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::RelativePath;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePath;
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
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
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
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::RelativePathBuf;
    /// use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
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
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
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

#[cfg(test)]
mod tests {
    use crate::fs::project_rel_path::ProjectRelativePath;
    use crate::fs::project_rel_path::ProjectRelativePathBuf;

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
    fn test_serde() {
        fn test_roundtrip(path: &str, json: &str) {
            let path = ProjectRelativePathBuf::try_from(path.to_owned()).unwrap();
            assert_eq!(json, serde_json::to_string(&path).unwrap());
            assert_eq!(
                path,
                serde_json::from_str::<ProjectRelativePathBuf>(json).unwrap()
            );
        }

        test_roundtrip("", r#""""#);
        test_roundtrip("foo", r#""foo""#);
        test_roundtrip("foo/bar", r#""foo/bar""#);

        let err = serde_json::from_str::<ProjectRelativePathBuf>(r#""a//b""#)
            .unwrap_err()
            .to_string();
        assert!(err.contains("expected a normalized path"), "{}", err);
    }
}
