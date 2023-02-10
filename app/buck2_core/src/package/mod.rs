/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
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

use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use fnv::FnvHasher;
use internment_tweaks::Equiv;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;

use crate::cells::cell_path::CellPath;
use crate::cells::cell_path::CellPathRef;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::cells::CellResolver;
use crate::fs::paths::fmt::quoted_display;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::project_rel_path::ProjectRelativePathBuf;

/// A 'Package' as defined above.
#[derive(
    Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
pub struct PackageLabel(Intern<PackageLabelData>);

#[derive(Debug, Display, Eq, PartialEq, Ord, PartialOrd, Allocative)]
struct PackageLabelData(CellPath);

#[derive(Hash, Eq, PartialEq)]
struct PackageLabelDataRef<'a> {
    cell: CellName,
    path: &'a CellRelativePath,
}

impl<'a> From<PackageLabelDataRef<'a>> for PackageLabelData {
    fn from(package_data: PackageLabelDataRef<'a>) -> Self {
        PackageLabelData(CellPath::new(package_data.cell, package_data.path.to_buf()))
    }
}

impl PackageLabelData {
    fn as_ref(&self) -> PackageLabelDataRef {
        PackageLabelDataRef {
            cell: self.0.cell(),
            path: self.0.path(),
        }
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for PackageLabelData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<'a> Equiv<PackageLabelData> for PackageLabelDataRef<'a> {
    fn equivalent(&self, key: &PackageLabelData) -> bool {
        self == &key.as_ref()
    }
}

static INTERNER: StaticInterner<PackageLabelData, FnvHasher> = StaticInterner::new();

impl PackageLabel {
    #[inline]
    pub fn new(cell: CellName, path: &CellRelativePath) -> Self {
        Self(INTERNER.intern(PackageLabelDataRef { cell, path }))
    }

    #[inline]
    pub fn from_cell_path(path: CellPathRef) -> Self {
        Self::new(path.cell(), path.path())
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
    pub fn as_cell_path(&self) -> CellPathRef {
        self.0.0.as_ref()
    }

    pub fn join(&self, path: &ForwardRelativePath) -> Self {
        if path.is_empty() {
            self.dupe()
        } else {
            PackageLabel::new(
                self.as_cell_path().cell(),
                &self.as_cell_path().path().join(path),
            )
        }
    }

    /// Some package name usable in tests.
    pub fn testing() -> PackageLabel {
        PackageLabel::new(
            CellName::unchecked_new("root"),
            CellRelativePath::new(ForwardRelativePath::new("package/subdir").unwrap()),
        )
    }
}

///
/// Resolves 'Package' to a corresponding 'ProjectRelativePath'
impl CellResolver {
    ///
    /// resolves a given 'Package' to the 'ProjectRelativePath' that points to
    /// the 'Package'
    ///
    /// ```
    /// use buck2_core::cells::CellResolver;
    /// use buck2_core::fs::project_rel_path::{ProjectRelativePath, ProjectRelativePathBuf};
    /// use buck2_core::fs::paths::forward_rel_path::{ForwardRelativePathBuf, ForwardRelativePath};
    /// use buck2_core::package::PackageLabel;
    /// use std::convert::TryFrom;
    /// use buck2_core::cells::cell_root_path::CellRootPathBuf;
    /// use buck2_core::cells::name::CellName;
    /// use buck2_core::cells::paths::CellRelativePath;
    /// use buck2_core::cells::testing::CellResolverExt;
    ///
    /// let cell_path = ProjectRelativePath::new("my/cell")?;
    ///
    /// let cells = CellResolver::of_names_and_paths(
    ///     CellName::unchecked_new("mycell"),
    ///     &[
    ///         (CellName::unchecked_new("mycell"), CellRootPathBuf::new(cell_path.to_buf()))
    ///     ]);
    ///
    /// let pkg = PackageLabel::new(
    ///     CellName::unchecked_new("mycell"),
    ///     CellRelativePath::unchecked_new("somepkg"),
    /// );
    ///
    /// assert_eq!(
    ///     cells.resolve_package(pkg)?,
    ///     ProjectRelativePathBuf::unchecked_new("my/cell/somepkg".into()),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve_package(&self, pkg: PackageLabel) -> anyhow::Result<ProjectRelativePathBuf> {
        self.resolve_path(pkg.0.0.as_ref())
    }
}

pub mod testing {
    use crate::cells::name::CellName;
    use crate::cells::paths::CellRelativePathBuf;
    use crate::package::PackageLabel;

    pub trait PackageExt {
        fn testing_new(cell: &str, path: &str) -> Self;
    }

    impl PackageExt for PackageLabel {
        fn testing_new(cell: &str, path: &str) -> Self {
            Self::new(
                CellName::unchecked_new(cell),
                &CellRelativePathBuf::unchecked_new(path.into()),
            )
        }
    }
}
