/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//!
//! A 'Package' in Buck corresponds to the subdirectories containing the
//! repository sources that are accessible to the targets defined in the build
//! file of current package. Each 'Package' can only contain one build file.
//!
//! A 'Package' is usually the entire directory contents where directory
//! contains a build file, including all transitive subdirectories that do not
//! contain a build file themselves, i.e. excluding all sub-packages. There's
//! also a set of outputs that corresponds to building all the targets of the
//! 'Package'.
//!
//! Example:
//! ```ignore
//! fbsource
//! +-- .buck
//! +-- package1
//! |   +-- TARGETS
//! |   +-- my.java
//! +-- package2
//! |   +-- subdir     // package 2 contains this subdir
//! |   |   +-- foo.cpp
//! |   +-- bar.cpp
//! |   +-- TARGETS
//! +-- package3
//! |   +-- package4  // package 3 excludes all subdirectories rooted at package4
//! |   |   +-- a.cpp
//! |   |   +-- TARGETS
//! |   +-- faz.java
//! |   +-- TARGETS
//! ```

pub mod package_relative_path;
pub mod source_path;

// Re-export for use in submodules
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
pub(crate) use buck2_fs::paths::fmt::quoted_display;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_util::hash::BuckHasher;
use derive_more::Display;
use dupe::Dupe;
use equivalent::Equivalent;
use pagable::Pagable;
use serde::Serialize;
use serde::Serializer;
use static_interner::Intern;
use static_interner::interner;
use strong_hash::StrongHash;

use crate::cells::cell_path::CellPath;
use crate::cells::cell_path::CellPathRef;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::pattern::pattern::Modifiers;

/// A 'Package' as defined above.
///
/// This type does not assert it represents a valid package.
/// However, we use it in context where we expect it to be a valid package
/// (for example, attempt to gather package listing for a package fails
/// if it is a directory, but does not have a build file).
///
/// A **valid** Buck2 package is defined by:
/// - A `BUCK` file that designates the root of the package.
/// - All files in the BUCK file’s directory and its subdirectories,
///   provided that none of those subdirectories contain their own `BUCK` file.
///   (If a subdirectory does contain a BUCK file, it forms a new, separate package.)
///
/// You can find the example above
///
/// a valid `PackageLabel` is the `CellPath` that points to a folder containing a `BUCK` file.
/// e.g. `root//path/to/package` is a valid `PackageLabel` if `root//path/to/package/BUCK` exists.
#[derive(
    Copy, Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative,
    StrongHash, Pagable
)]
pub struct PackageLabel(Intern<PackageLabelData>);

impl Serialize for PackageLabel {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&self.to_string())
    }
}

#[derive(Dupe, Clone, Eq, PartialEq, Hash, Debug, Ord, PartialOrd)]
pub struct PackageLabelWithModifiers {
    pub package: PackageLabel,
    pub modifiers: Modifiers,
}

#[derive(
    Debug, Display, Eq, PartialEq, Ord, PartialOrd, Allocative, Pagable, StrongHash
)]
struct PackageLabelData(CellPath);

#[derive(Hash, Eq, PartialEq)]
struct PackageLabelDataRef<'a> {
    path: CellPathRef<'a>,
}

impl<'a> From<PackageLabelDataRef<'a>> for PackageLabelData {
    fn from(package_data: PackageLabelDataRef<'a>) -> Self {
        PackageLabelData(package_data.path.to_owned())
    }
}

impl PackageLabelData {
    fn as_ref(&self) -> PackageLabelDataRef<'_> {
        PackageLabelDataRef {
            path: self.0.as_ref(),
        }
    }
}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for PackageLabelData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl Equivalent<PackageLabelData> for PackageLabelDataRef<'_> {
    fn equivalent(&self, key: &PackageLabelData) -> bool {
        self == &key.as_ref()
    }
}

interner!(INTERNER, BuckHasher, PackageLabelData);

impl PackageLabel {
    #[inline]
    pub fn new(cell: CellName, path: &CellRelativePath) -> buck2_error::Result<Self> {
        PackageLabel::from_cell_path(CellPathRef::new(cell, path))
    }

    #[inline]
    pub fn from_cell_path(path: CellPathRef) -> buck2_error::Result<Self> {
        Ok(PackageLabel(INTERNER.intern(PackageLabelDataRef { path })))
    }

    #[inline]
    pub fn cell_name(&self) -> CellName {
        self.0.0.cell()
    }

    #[inline]
    pub fn cell_relative_path(&self) -> &'static CellRelativePath {
        self.0.deref_static().0.path()
    }

    #[inline]
    pub fn to_cell_path(&self) -> CellPath {
        self.0.0.clone()
    }

    #[inline]
    pub fn as_cell_path(&self) -> CellPathRef<'_> {
        self.0.0.as_ref()
    }

    pub fn join(&self, path: &ForwardRelativePath) -> buck2_error::Result<Self> {
        if path.is_empty() {
            Ok(self.dupe())
        } else {
            PackageLabel::from_cell_path(self.as_cell_path().join(path).as_ref())
        }
    }

    pub fn parent(&self) -> buck2_error::Result<Option<PackageLabel>> {
        match self.as_cell_path().parent() {
            Some(parent) => PackageLabel::from_cell_path(parent).map(Some),
            None => Ok(None),
        }
    }

    // Following functions should only be used in tests, so they have "testing" in their names.

    /// Some package name usable in tests.
    pub fn testing() -> PackageLabel {
        PackageLabel::new(
            CellName::testing_new("root"),
            CellRelativePath::new(ForwardRelativePath::new("package/subdir").unwrap()),
        )
        .unwrap()
    }

    pub fn testing_new(cell: &str, path: &str) -> PackageLabel {
        PackageLabel::new(
            CellName::testing_new(cell),
            CellRelativePath::new(ForwardRelativePath::new(path).unwrap()),
        )
        .unwrap()
    }

    pub fn testing_parse(label: &str) -> PackageLabel {
        let (cell, path) = label.split_once("//").unwrap();
        PackageLabel::testing_new(cell, path)
    }
}

#[cfg(test)]
mod tests {
    use crate::package::PackageLabel;

    #[test]
    fn test_serialize() {
        assert_eq!(
            "foo//bar/baz",
            PackageLabel::testing_parse("foo//bar/baz").to_string()
        );
    }
}
