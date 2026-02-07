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
use buck2_error::internal_error;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use dupe::Dupe;
use pagable::Pagable;
use relative_path::RelativePath;
use strong_hash::StrongHash;

use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::cells::paths::CellRelativePathBuf;

#[derive(buck2_error::Error, Debug)]
#[error("attempted to strip prefix of two CellPath with different cell names `{0}` and `{1}`")]
#[buck2(tag = Tier0)]
struct StripPrefixError(CellName, CellName);

/// Represents a resolvable path corresponding to some path that is relative to the cell
/// corresponding to the 'CellName'.
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative,
    StrongHash,
    Pagable
)]
#[display("{}", self.as_ref())]
pub struct CellPath {
    cell: CellName,
    path: Box<CellRelativePath>,
}

impl CellPath {
    #[inline]
    pub fn new(cell: CellName, path: CellRelativePathBuf) -> Self {
        CellPath {
            cell,
            path: path.into_box(),
        }
    }

    #[inline]
    pub fn cell(&self) -> CellName {
        self.cell
    }

    #[inline]
    pub fn path(&self) -> &CellRelativePath {
        &self.path
    }

    /// Creates an owned 'CellRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
    ///
    /// let path = CellPath::new(
    ///     CellName::testing_new("cell"),
    ///     CellRelativePathBuf::unchecked_new("foo/bar".into()),
    /// );
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo/bar/baz".into())
    ///     ),
    ///     path.join(other)
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellPath {
        self.as_ref().join(path)
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    ///
    /// assert_eq!(
    ///     Some(CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo".into())
    ///     )),
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     )
    ///     .parent()
    ///     .map(|p| p.to_owned()),
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn parent(&self) -> Option<CellPathRef<'_>> {
        self.as_ref().parent()
    }

    /// Produces an iterator over CellPath and its ancestors.
    ///
    /// The iterator will yield all the CellPaths that is returned if
    /// the parent method is used zero or more times in order.
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    ///
    /// let path = CellPath::testing_new("cell//foo/bar");
    /// let mut ancestors = path.ancestors();
    ///
    /// assert_eq!(
    ///     ancestors.next(),
    ///     Some(CellPath::testing_new("cell//foo/bar").as_ref())
    /// );
    /// assert_eq!(
    ///     ancestors.next(),
    ///     Some(CellPath::testing_new("cell//foo").as_ref())
    /// );
    /// assert_eq!(
    ///     ancestors.next(),
    ///     Some(CellPath::testing_new("cell//").as_ref())
    /// );
    /// assert_eq!(ancestors.next(), None);
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn ancestors(&self) -> impl Iterator<Item = CellPathRef<'_>> {
        self.as_ref().ancestors()
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`.
    ///
    /// Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
    ///
    /// let path = CellPath::new(
    ///     CellName::testing_new("cell"),
    ///     CellRelativePathBuf::unchecked_new("test/haha/foo.txt".into()),
    /// );
    ///
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         CellPath::new(
    ///             CellName::testing_new("cell"),
    ///             CellRelativePathBuf::unchecked_new("test".into()),
    ///         )
    ///         .as_ref()
    ///     )?,
    ///     ForwardRelativePathBuf::unchecked_new("haha/foo.txt".into())
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         CellPath::new(
    ///             CellName::testing_new("cell"),
    ///             CellRelativePathBuf::unchecked_new("asdf".into()),
    ///         )
    ///         .as_ref()
    ///     )
    ///     .is_err(),
    ///     true
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(
    ///         CellPath::new(
    ///             CellName::testing_new("another"),
    ///             CellRelativePathBuf::unchecked_new("test".into()),
    ///         )
    ///         .as_ref()
    ///     )
    ///     .is_err(),
    ///     true
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn strip_prefix<'a>(
        &'a self,
        base: CellPathRef,
    ) -> buck2_error::Result<&'a ForwardRelativePath> {
        self.as_ref().strip_prefix(base)
    }

    /// Build an owned `CellPath`, joined with the given path and
    /// normalized.
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    ///
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     )
    ///     .join_normalized("../baz.txt")?,
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo/baz.txt".into())
    ///     ),
    /// );
    ///
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo".into())
    ///     )
    ///     .join_normalized("../../baz.txt")
    ///     .is_err(),
    ///     true
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(
        &self,
        path: P,
    ) -> buck2_error::Result<CellPath> {
        Ok(CellPath::new(self.cell, self.path.join_normalized(path)?))
    }

    /// Checks that cell matches and `self` path starts with `base` path
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    ///
    /// assert!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePathBuf::unchecked_new("foo/bar".into())
    ///     )
    ///     .starts_with(
    ///         CellPath::new(
    ///             CellName::testing_new("cell"),
    ///             CellRelativePathBuf::unchecked_new("foo".into())
    ///         )
    ///         .as_ref()
    ///     ),
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn starts_with(&self, base: CellPathRef) -> bool {
        self.as_ref().starts_with(base)
    }

    #[inline]
    pub fn ends_with(&self, suffix: &ForwardRelativePath) -> bool {
        self.as_ref().ends_with(suffix)
    }

    #[inline]
    pub fn into_parts(self) -> (CellName, Box<CellRelativePath>) {
        (self.cell, self.path)
    }

    pub fn testing_new(path: &str) -> CellPath {
        let (cell_name, relative_path) = path.split_once("//").unwrap();
        CellPath::new(
            CellName::testing_new(cell_name),
            CellRelativePathBuf::testing_new(relative_path),
        )
    }

    #[inline]
    pub fn as_ref(&self) -> CellPathRef<'_> {
        CellPathRef {
            cell: self.cell,
            path: &self.path,
        }
    }
}

#[derive(Debug, Clone, Dupe, Copy, Eq, PartialEq, Hash, derive_more::Display)]
#[display("{}//{}", cell, path)]
pub struct CellPathRef<'a> {
    cell: CellName,
    path: &'a CellRelativePath,
}

impl<'a> CellPathRef<'a> {
    #[inline]
    pub fn new(cell: CellName, path: &'a CellRelativePath) -> CellPathRef<'a> {
        CellPathRef { cell, path }
    }

    pub fn testing_new(path: &str) -> CellPathRef<'_> {
        let (cell, path) = path
            .split_once("//")
            .ok_or_else(|| internal_error!("invalid path: `{path}`"))
            .unwrap();
        CellPathRef {
            cell: CellName::testing_new(cell),
            path: CellRelativePath::testing_new(path),
        }
    }

    #[inline]
    pub fn parent(&self) -> Option<CellPathRef<'a>> {
        Some(CellPathRef {
            cell: self.cell,
            path: self.path.parent()?,
        })
    }

    #[inline]
    pub fn to_owned(&self) -> CellPath {
        CellPath {
            cell: self.cell,
            path: self.path.to_box(),
        }
    }

    #[inline]
    pub fn cell(&self) -> CellName {
        self.cell
    }

    #[inline]
    pub fn path(&self) -> &'a CellRelativePath {
        self.path
    }

    pub fn ancestors(&self) -> impl Iterator<Item = CellPathRef<'a>> + use<'a> {
        struct Ancestors<'a>(Option<CellPathRef<'a>>);
        impl<'a> Iterator for Ancestors<'a> {
            type Item = CellPathRef<'a>;

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

        Ancestors(Some(self.dupe()))
    }

    #[inline]
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellPath {
        CellPath {
            cell: self.cell,
            path: self.path.join(path).into_box(),
        }
    }

    #[inline]
    pub fn starts_with(&self, base: CellPathRef) -> bool {
        self.cell() == base.cell() && self.path().starts_with(base.path())
    }

    #[inline]
    pub fn ends_with(&self, suffix: &ForwardRelativePath) -> bool {
        self.path().ends_with(suffix)
    }

    #[inline]
    pub fn strip_prefix(&self, base: CellPathRef) -> buck2_error::Result<&'a ForwardRelativePath> {
        if self.cell != base.cell {
            return Err(StripPrefixError(self.cell, base.cell).into());
        }
        self.path.strip_prefix(&base.path)
    }
}

pub(crate) enum CellPathCow<'a> {
    Borrowed(CellPathRef<'a>),
    Owned(CellPath),
}

impl CellPathCow<'_> {
    pub(crate) fn into_owned(self) -> CellPath {
        match self {
            CellPathCow::Borrowed(v) => v.to_owned(),
            CellPathCow::Owned(v) => v,
        }
    }

    pub(crate) fn as_ref(&self) -> CellPathRef<'_> {
        match self {
            CellPathCow::Borrowed(v) => *v,
            CellPathCow::Owned(v) => v.as_ref(),
        }
    }
}
