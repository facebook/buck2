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
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::cells::i_cell_relative_path::ICellRelativePath;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;

#[derive(buck2_error::Error, Debug)]
#[error("attempted to strip prefix of two CellPath with different cell names `{0}` and `{1}`")]
#[buck2(tag = Tier0)]
struct StripPrefixError(CellName, CellName);

/// Represents a resolvable path corresponding to some path that is relative to the cell
/// corresponding to the 'CellName'.
#[derive(
    Copy,
    Clone,
    Dupe,
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
#[display("{cell}//{path}")]
pub struct CellPath {
    cell: CellName,
    path: ICellRelativePath,
}

impl CellPath {
    #[inline]
    pub fn new(cell: CellName, path: &CellRelativePath) -> Self {
        CellPath {
            cell,
            path: ICellRelativePath::intern_directly(path),
        }
    }

    /// A `CellPath` at the root of the given cell (empty relative path).
    #[inline]
    pub fn empty(cell: CellName) -> Self {
        CellPath {
            cell,
            path: ICellRelativePath::empty(),
        }
    }

    pub fn testing_new(path: &str) -> CellPath {
        let (cell_name, relative_path) = path.split_once("//").unwrap();
        CellPath::new(
            CellName::testing_new(cell_name),
            CellRelativePath::testing_new(relative_path),
        )
    }

    #[inline]
    pub fn cell(self) -> CellName {
        self.cell
    }

    #[inline]
    pub fn path(self) -> &'static CellRelativePath {
        self.path.as_cell_relative_path()
    }

    /// Creates a 'CellPath' with `path` adjoined to self.
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
    ///
    /// let path = CellPath::new(
    ///     CellName::testing_new("cell"),
    ///     CellRelativePath::unchecked_new("foo/bar"),
    /// );
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("foo/bar/baz")
    ///     ),
    ///     path.join(other)
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn join<P: AsRef<ForwardRelativePath>>(self, path: P) -> CellPath {
        CellPath {
            cell: self.cell,
            path: ICellRelativePath::intern_concat_directly([
                self.path.as_forward_relative_path(),
                path.as_ref(),
            ]),
        }
    }

    /// Returns the parent directory.
    ///
    /// Allocation-free (does not re-intern).
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// assert_eq!(
    ///     Some(CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("foo")
    ///     )),
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("foo/bar")
    ///     )
    ///     .parent(),
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn parent(self) -> Option<CellPath> {
        Some(CellPath {
            cell: self.cell,
            path: self.path.parent()?,
        })
    }

    /// Produces an iterator over CellPath and its ancestors.
    ///
    /// The iterator will yield all the CellPaths that is returned if
    /// the parent method is used zero or more times in order.
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    ///
    /// let path = CellPath::testing_new("cell//foo/bar");
    /// let mut ancestors = path.ancestors();
    ///
    /// assert_eq!(
    ///     ancestors.next(),
    ///     Some(CellPath::testing_new("cell//foo/bar"))
    /// );
    /// assert_eq!(ancestors.next(), Some(CellPath::testing_new("cell//foo")));
    /// assert_eq!(ancestors.next(), Some(CellPath::testing_new("cell//")));
    /// assert_eq!(ancestors.next(), None);
    ///
    /// # buck2_error::Ok(())
    /// ```
    pub fn ancestors(self) -> impl Iterator<Item = CellPath> {
        struct Ancestors(Option<CellPath>);
        impl Iterator for Ancestors {
            type Item = CellPath;

            fn next(&mut self) -> Option<Self::Item> {
                let current = self.0?;
                self.0 = current.parent();
                Some(current)
            }
        }

        Ancestors(Some(self))
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
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
    ///
    /// let path = CellPath::new(
    ///     CellName::testing_new("cell"),
    ///     CellRelativePath::unchecked_new("test/haha/foo.txt"),
    /// );
    ///
    /// assert_eq!(
    ///     path.strip_prefix(CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("test"),
    ///     ))?,
    ///     ForwardRelativePathBuf::unchecked_new("haha/foo.txt".into())
    /// );
    /// assert!(
    ///     path.strip_prefix(CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("asdf"),
    ///     ))
    ///     .is_err()
    /// );
    /// assert!(
    ///     path.strip_prefix(CellPath::new(
    ///         CellName::testing_new("another"),
    ///         CellRelativePath::unchecked_new("test"),
    ///     ))
    ///     .is_err()
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn strip_prefix(self, base: CellPath) -> buck2_error::Result<&'static ForwardRelativePath> {
        if self.cell != base.cell {
            return Err(StripPrefixError(self.cell, base.cell).into());
        }
        self.path
            .as_cell_relative_path()
            .strip_prefix(base.path.as_cell_relative_path())
    }

    /// Checks that cell matches and `self` path starts with `base` path.
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// assert!(
    ///     CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("foo/bar")
    ///     )
    ///     .starts_with(CellPath::new(
    ///         CellName::testing_new("cell"),
    ///         CellRelativePath::unchecked_new("foo")
    ///     )),
    /// );
    ///
    /// # buck2_error::Ok(())
    /// ```
    #[inline]
    pub fn starts_with(self, base: CellPath) -> bool {
        self.cell == base.cell && self.path().starts_with(base.path())
    }

    #[inline]
    pub fn ends_with(self, suffix: &ForwardRelativePath) -> bool {
        self.path().ends_with(suffix)
    }
}
