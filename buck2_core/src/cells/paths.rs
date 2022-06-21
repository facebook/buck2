/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Paths relative to 'Cell's
//! 'CellRelativePath' and the owned 'CellRelativePathBuf' are used to represent paths that are
//! relative to a cell.
//! There's also a 'CellPath', which is a 'CellName' and 'CellRelativePathBuf' pair that
//! represents a resolvable path, which can be resolved into a 'ProjectRelativePath'.

use std::borrow::Borrow;
use std::convert::TryFrom;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use derivative::Derivative;
use derive_more::Display;
use ref_cast::RefCast;
use relative_path::RelativePath;
use serde::Serialize;
use thiserror::Error;

use crate::cells::CellName;
use crate::fs::paths::fmt::quoted_display;
use crate::fs::paths::FileName;
use crate::fs::paths::ForwardRelativePath;
use crate::fs::paths::ForwardRelativePathBuf;
use crate::fs::paths::ForwardRelativePathIter;
use crate::fs::paths::RelativePathBuf;

/// Represents a resolvable path corresponding to some path that is relative to the cell
/// corresponding to the 'CellName'.
#[derive(Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}//{}", cell, path)]
pub struct CellPath {
    cell: CellName,
    path: CellRelativePathBuf,
}

impl CellPath {
    pub fn new(cell: CellName, path: CellRelativePathBuf) -> Self {
        CellPath { cell, path }
    }

    pub fn cell(&self) -> &CellName {
        &self.cell
    }

    pub fn path(&self) -> &CellRelativePath {
        &self.path
    }

    /// Creates an owned 'CellRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use buck2_core::cells::paths::{CellPath, CellRelativePathBuf};
    /// use buck2_core::cells::CellName;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    ///
    /// let path = CellPath::new(
    ///     CellName::unchecked_new("cell".into()),
    ///     CellRelativePathBuf::unchecked_new("foo/bar".into())
    /// );
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(
    ///     CellPath::new(CellName::unchecked_new("cell".into()),
    ///     CellRelativePathBuf::unchecked_new("foo/bar/baz".into())), path.join_unnormalized(other)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_unnormalized<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellPath {
        CellPath::new(
            self.cell.clone(),
            self.path.join_unnormalized(path.as_ref()),
        )
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::cells::paths::{CellPath, CellRelativePathBuf};
    /// use buck2_core::cells::CellName;
    ///
    /// assert_eq!(
    ///     Some(
    ///         CellPath::new(CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo".into()))
    ///     ),
    ///     CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     ).parent(),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn parent(&self) -> Option<CellPath> {
        self.path
            .parent()
            .map(|p| CellPath::new(self.cell.clone(), p.to_buf()))
    }

    /// Produces an iterator over CellPath and its ancestors.
    ///
    /// The iterator will yield all the CellPaths that is returned if
    /// the parent method is used zero or more times in order.
    ///
    /// ```
    /// use buck2_core::cells::paths::{CellPath, CellRelativePathBuf};
    /// use buck2_core::cells::CellName;
    ///
    /// let path = CellPath::testing_new("cell", "foo/bar");
    /// let mut ancestors = path.ancestors();
    ///
    /// assert_eq!(ancestors.next(), Some(CellPath::testing_new("cell", "foo/bar")));
    /// assert_eq!(ancestors.next(), Some(CellPath::testing_new("cell", "foo")));
    /// assert_eq!(ancestors.next(), Some(CellPath::testing_new("cell", "")));
    /// assert_eq!(ancestors.next(), None);
    ///
    /// # anyhow::Ok(())
    /// ```
    // TODO(nga): iterate without allocation
    pub fn ancestors(&self) -> impl Iterator<Item = CellPath> {
        struct Ancestors(Option<CellPath>);
        impl Iterator for Ancestors {
            type Item = CellPath;

            fn next(&mut self) -> Option<Self::Item> {
                match &self.0 {
                    None => None,
                    Some(v) => {
                        let mut next = v.parent();
                        std::mem::swap(&mut next, &mut self.0);
                        next
                    }
                }
            }
        }

        Ancestors(Some(self.clone()))
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`.
    ///
    /// Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::cells::paths::{CellPath, CellRelativePathBuf};
    /// use buck2_core::cells::CellName;
    /// use buck2_core::fs::paths::ForwardRelativePathBuf;
    ///
    /// let path = CellPath::new(
    ///     CellName::unchecked_new("cell".into()),
    ///     CellRelativePathBuf::unchecked_new("test/haha/foo.txt".into())
    /// );
    ///
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         &CellPath::new(
    ///             CellName::unchecked_new("cell".into()),
    ///             CellRelativePathBuf::unchecked_new("test".into()),
    ///         )
    ///     )?,
    ///     ForwardRelativePathBuf::unchecked_new("haha/foo.txt".into())
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         &CellPath::new(
    ///             CellName::unchecked_new("cell".into()),
    ///             CellRelativePathBuf::unchecked_new("asdf".into()),
    ///         )
    ///     ).is_err(),
    ///     true
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         &CellPath::new(
    ///             CellName::unchecked_new("another".into()),
    ///             CellRelativePathBuf::unchecked_new("test".into()),
    ///         )
    ///     ).is_err(),
    ///     true
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn strip_prefix(&self, base: &CellPath) -> anyhow::Result<&ForwardRelativePath> {
        if self.cell != base.cell {
            return Err(anyhow!(StripPrefixError(
                self.cell.clone(),
                base.cell.clone()
            )));
        }
        self.path.strip_prefix(&base.path)
    }

    /// Build an owned `CellPath`, joined with the given path and
    /// normalized.
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::{CellRelativePathBuf, CellPath};
    /// use buck2_core::cells::CellName;
    /// use std::convert::TryFrom;
    ///
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     ).join_normalized("../baz.txt")?,
    ///     CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo/baz.txt".into())
    ///     ),
    /// );
    ///
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo".into())
    ///     ).join_normalized("../../baz.txt").is_err(),
    ///     true
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(&self, path: P) -> anyhow::Result<CellPath> {
        Ok(CellPath::new(
            self.cell.clone(),
            self.path.join_normalized(path)?,
        ))
    }

    /// Checks that cell matches and `self` path starts with `base` path
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::{CellRelativePathBuf, CellPath};
    /// use buck2_core::cells::CellName;
    /// use std::convert::TryFrom;
    ///
    /// assert!(
    ///     CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     ).starts_with(&CellPath::new(
    ///         CellName::unchecked_new("cell".into()),
    ///         CellRelativePathBuf::unchecked_new("foo".into())
    ///     )),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn starts_with(&self, base: &CellPath) -> bool {
        self.cell() == base.cell() && self.path().starts_with(base.path())
    }

    pub fn into_parts(self) -> (CellName, CellRelativePathBuf) {
        (self.cell, self.path)
    }

    pub fn testing_new(cell_name: &str, relative_path: &str) -> CellPath {
        CellPath::new(
            CellName::unchecked_new(cell_name.into()),
            CellRelativePathBuf::unchecked_new(relative_path.into()),
        )
    }
}

#[derive(Error, Debug)]
#[error("attempted to strip prefix of two CellPath with different cell names `{0}` and `{1}`")]
pub struct StripPrefixError(CellName, CellName);

/// A un-owned forward pointing, fully normalized path that is relative to the cell
#[derive(Display, Derivative, Hash, PartialEq, Eq, RefCast, Ord, PartialOrd)]
#[derivative(Debug)]
#[repr(transparent)]
pub struct CellRelativePath(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePath,
);

/// The owned version of the 'CellRelativePath'
#[derive(Clone, Display, Derivative)]
// split in two lines because formatters disagree
#[derive(Hash, PartialEq, Eq, Ord, PartialOrd, Serialize)]
#[derivative(Debug)]
pub struct CellRelativePathBuf(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePathBuf,
);

impl AsRef<ForwardRelativePath> for CellRelativePath {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for CellRelativePath {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePath> for CellRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for CellRelativePathBuf {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePathBuf> for CellRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePathBuf {
        &self.0
    }
}

impl CellRelativePath {
    pub fn unchecked_new<S: ?Sized + AsRef<str>>(s: &S) -> &Self {
        CellRelativePath::ref_cast(ForwardRelativePath::unchecked_new(s))
    }

    /// Creates a 'CellRelativePath' if the given string represents a
    /// forward, normalized relative path, otherwise error.
    ///
    /// ```
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use std::path::Path;
    ///
    /// assert!(CellRelativePath::from_path("foo/bar").is_ok());
    /// assert!(CellRelativePath::from_path("").is_ok());
    /// assert!(CellRelativePath::from_path("/abs/bar").is_err());
    /// assert!(CellRelativePath::from_path("normalize/./bar").is_err());
    /// assert!(CellRelativePath::from_path("normalize/../bar").is_err());
    ///
    /// assert!(CellRelativePath::from_path(Path::new("foo/bar")).is_ok());
    /// assert!(CellRelativePath::from_path(Path::new("")).is_ok());
    /// assert!(CellRelativePath::from_path(Path::new("/abs/bar")).is_err());
    /// assert!(CellRelativePath::from_path(Path::new("normalize/./bar")).is_err());
    /// assert!(CellRelativePath::from_path(Path::new("normalize/../bar")).is_err());
    /// ```
    pub fn from_path<P: ?Sized + AsRef<Path>>(p: &P) -> anyhow::Result<&CellRelativePath> {
        Ok(CellRelativePath::ref_cast(ForwardRelativePath::new(p)?))
    }

    pub fn new(path: &ForwardRelativePath) -> &CellRelativePath {
        CellRelativePath::ref_cast(path)
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_forward_relative_path(&self) -> &ForwardRelativePath {
        &self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates an owned 'CellRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::cells::paths::{CellRelativePathBuf, CellRelativePath};
    ///
    /// let path = CellRelativePath::from_path("foo/bar")?;
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(CellRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()), path.join_unnormalized(other));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_unnormalized<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellRelativePathBuf {
        CellRelativePathBuf(self.0.join_unnormalized(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// assert_eq!(
    ///     Some(CellRelativePath::from_path("foo")?),
    ///     CellRelativePath::from_path("foo/bar")?.parent()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn parent(&self) -> Option<&CellRelativePath> {
        self.0.parent().map(CellRelativePath::ref_cast)
    }

    /// Returns the final component of the `CellRelativePath`, if there is
    /// one.
    ///
    /// If the path is a normal file, this is the file name. If it's the path of
    /// a directory, this is the directory name.
    ///
    /// ```
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// assert_eq!(Some(FileName::unchecked_new("bin")), CellRelativePath::from_path("usr/bin")?.file_name());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_name(&self) -> Option<&FileName> {
        self.0.file_name().map(FileName::unchecked_new)
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`.
    ///
    /// Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// let path = CellRelativePath::from_path("test/haha/foo.txt")?;
    ///
    /// assert_eq!(
    ///     path.strip_prefix(CellRelativePath::from_path("test")?)?,
    ///     ForwardRelativePath::new("haha/foo.txt")?
    /// );
    /// assert_eq!(path.strip_prefix(CellRelativePath::from_path("asdf")?).is_err(), true);
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn strip_prefix<P: ?Sized>(&self, base: &P) -> anyhow::Result<&ForwardRelativePath>
    where
        P: AsRef<CellRelativePath>,
    {
        self.0.strip_prefix(&base.as_ref().0)
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// let path = CellRelativePath::from_path("some/foo")?;
    ///
    /// assert!(path.starts_with(CellRelativePath::from_path("some")?));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn starts_with<P: AsRef<CellRelativePath>>(&self, base: P) -> bool {
        self.0.starts_with(&base.as_ref().0)
    }

    /// Determines whether `child` is a suffix of `self`.
    /// Only considers whole path components to match.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    ///
    /// let path = CellRelativePath::from_path("some/foo")?;
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
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// let path = CellRelativePath::from_path("foo.rs")?;
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
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// assert_eq!(Some("rs"), CellRelativePath::from_path("hi/foo.rs")?.extension());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    /// Build an owned `CellRelativePathBuf`, joined with the given path and
    /// normalized.
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::{CellRelativePath, CellRelativePathBuf};
    /// use std::convert::TryFrom;
    ///
    /// assert_eq!(
    ///     CellRelativePath::from_path("foo/bar")?.join_normalized("../baz.txt")?,
    ///     CellRelativePathBuf::unchecked_new("foo/baz.txt".into()),
    /// );
    ///
    /// assert_eq!(
    ///     CellRelativePath::from_path("foo")?.join_normalized("../../baz.txt").is_err(),
    ///     true
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(
        &self,
        path: P,
    ) -> anyhow::Result<CellRelativePathBuf> {
        let inner = self.0.join_normalized(path)?;
        // TODO need verify?
        Ok(CellRelativePathBuf(inner))
    }

    /// Iterator over the components of this path
    ///
    /// ```
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// let p = CellRelativePath::from_path("foo/bar/baz")?;
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

    pub fn to_buf(&self) -> CellRelativePathBuf {
        self.to_owned()
    }
}

impl<'a> From<&'a ForwardRelativePath> for &'a CellRelativePath {
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use std::convert::From;
    ///
    /// let f = ForwardRelativePath::new("foo")?;
    ///
    /// assert_eq!(<&CellRelativePath>::from(f), CellRelativePath::from_path("foo")?);
    ///
    /// # anyhow::Ok(())
    /// ```
    fn from(p: &'a ForwardRelativePath) -> &'a CellRelativePath {
        CellRelativePath::ref_cast(p)
    }
}

impl CellRelativePathBuf {
    pub fn unchecked_new(s: String) -> Self {
        Self(ForwardRelativePathBuf::unchecked_new(s))
    }

    /// Creates a new 'CellRelativePathBuf' with a given capacity used to create the internal
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
    pub fn push_unnormalized<P: AsRef<ForwardRelativePath>>(&mut self, path: P) {
        self.0.push_unnormalized(path)
    }

    /// Pushes a `RelativePath` to the existing buffer, normalizing it
    pub fn push_normalized<P: AsRef<RelativePath>>(&mut self, path: P) -> anyhow::Result<()> {
        self.0.push_normalized(path)
    }
}

impl From<ForwardRelativePathBuf> for CellRelativePathBuf {
    fn from(p: ForwardRelativePathBuf) -> Self {
        Self(p)
    }
}

impl From<CellRelativePathBuf> for ForwardRelativePathBuf {
    fn from(p: CellRelativePathBuf) -> Self {
        p.0
    }
}

impl<'a> TryFrom<&'a str> for &'a CellRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    ///
    /// assert!(<&CellRelativePath>::try_from("foo/bar").is_ok());
    /// assert!(<&CellRelativePath>::try_from("").is_ok());
    /// assert!(<&CellRelativePath>::try_from("/abs/bar").is_err());
    /// assert!(<&CellRelativePath>::try_from("normalize/./bar").is_err());
    /// assert!(<&CellRelativePath>::try_from("normalize/../bar").is_err());
    /// ```
    fn try_from(s: &'a str) -> anyhow::Result<&'a CellRelativePath> {
        Ok(CellRelativePath::ref_cast(ForwardRelativePath::new(s)?))
    }
}

impl<'a> TryFrom<&'a RelativePath> for &'a CellRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::RelativePath;
    ///
    /// assert!(<&CellRelativePath>::try_from(RelativePath::new("foo/bar")).is_ok());
    /// assert!(<&CellRelativePath>::try_from(RelativePath::new("")).is_ok());
    /// assert!(<&CellRelativePath>::try_from(RelativePath::new("normalize/./bar")).is_err());
    /// assert!(<&CellRelativePath>::try_from(RelativePath::new("normalize/../bar")).is_err());
    /// ```
    fn try_from(s: &'a RelativePath) -> anyhow::Result<&'a CellRelativePath> {
        Ok(CellRelativePath::ref_cast(ForwardRelativePath::new(
            s.as_str(),
        )?))
    }
}

impl TryFrom<String> for CellRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(CellRelativePathBuf::try_from("foo/bar".to_owned()).is_ok());
    /// assert!(CellRelativePathBuf::try_from("".to_owned()).is_ok());
    /// assert!(CellRelativePathBuf::try_from("/abs/bar".to_owned()).is_err());
    /// assert!(CellRelativePathBuf::try_from("normalize/./bar".to_owned()).is_err());
    /// assert!(CellRelativePathBuf::try_from("normalize/../bar".to_owned()).is_err());
    /// ```
    fn try_from(s: String) -> anyhow::Result<CellRelativePathBuf> {
        Ok(CellRelativePathBuf::from(ForwardRelativePathBuf::try_from(
            s,
        )?))
    }
}

impl TryFrom<RelativePathBuf> for CellRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion (TODO make ForwardRelativePath a no allocation
    /// conversion)
    ///
    /// ```
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_core::fs::paths::RelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(CellRelativePathBuf::try_from(RelativePathBuf::from("foo/bar")).is_ok());
    /// assert!(CellRelativePathBuf::try_from(RelativePathBuf::from("")).is_ok());
    /// assert!(CellRelativePathBuf::try_from(RelativePathBuf::from("normalize/./bar")).is_err());
    /// assert!(CellRelativePathBuf::try_from(RelativePathBuf::from("normalize/../bar")).is_err());
    /// ```
    fn try_from(p: RelativePathBuf) -> anyhow::Result<CellRelativePathBuf> {
        Ok(CellRelativePathBuf::from(ForwardRelativePathBuf::try_from(
            p,
        )?))
    }
}

impl TryFrom<PathBuf> for CellRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// assert!(CellRelativePathBuf::try_from(PathBuf::from("foo/bar")).is_ok());
    /// assert!(CellRelativePathBuf::try_from(PathBuf::from("")).is_ok());
    /// assert!(CellRelativePathBuf::try_from(PathBuf::from("/abs/bar")).is_err());
    /// assert!(CellRelativePathBuf::try_from(PathBuf::from("normalize/./bar")).is_err());
    /// assert!(CellRelativePathBuf::try_from(PathBuf::from("normalize/../bar")).is_err());
    /// ```
    fn try_from(p: PathBuf) -> anyhow::Result<CellRelativePathBuf> {
        Ok(CellRelativePathBuf(ForwardRelativePathBuf::try_from(p)?))
    }
}

impl ToOwned for CellRelativePath {
    type Owned = CellRelativePathBuf;

    fn to_owned(&self) -> CellRelativePathBuf {
        CellRelativePathBuf(self.0.to_owned())
    }
}

impl AsRef<CellRelativePath> for CellRelativePath {
    fn as_ref(&self) -> &CellRelativePath {
        self
    }
}

impl AsRef<CellRelativePath> for CellRelativePathBuf {
    fn as_ref(&self) -> &CellRelativePath {
        CellRelativePath::ref_cast(&self.0)
    }
}

impl Borrow<CellRelativePath> for CellRelativePathBuf {
    fn borrow(&self) -> &CellRelativePath {
        self.as_ref()
    }
}

impl Deref for CellRelativePathBuf {
    type Target = CellRelativePath;

    fn deref(&self) -> &CellRelativePath {
        CellRelativePath::ref_cast(&self.0)
    }
}
