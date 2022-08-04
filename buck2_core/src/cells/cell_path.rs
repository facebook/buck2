/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use relative_path::RelativePath;

use crate::cells::paths::CellRelativePath;
use crate::cells::paths::CellRelativePathBuf;
use crate::cells::CellName;
use crate::fs::paths::ForwardRelativePath;

#[derive(thiserror::Error, Debug)]
#[error("attempted to strip prefix of two CellPath with different cell names `{0}` and `{1}`")]
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
    PartialOrd
)]
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
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::paths::{CellRelativePathBuf};
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
    ///     CellRelativePathBuf::unchecked_new("foo/bar/baz".into())), path.join(other)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> CellPath {
        CellPath::new(self.cell.clone(), self.path.join(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::paths::{CellRelativePathBuf};
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
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::paths::{CellRelativePathBuf};
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
    /// use buck2_core::cells::cell_path::CellPath;
    /// use buck2_core::cells::paths::{CellRelativePathBuf};
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
            return Err(StripPrefixError(self.cell.clone(), base.cell.clone()).into());
        }
        self.path.strip_prefix(&base.path)
    }

    /// Build an owned `CellPath`, joined with the given path and
    /// normalized.
    ///
    /// ```
    ///
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_core::cells::CellName;
    /// use std::convert::TryFrom;
    /// use buck2_core::cells::cell_path::CellPath;
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
    /// use buck2_core::cells::paths::CellRelativePathBuf;
    /// use buck2_core::cells::cell_path::CellPath;
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
