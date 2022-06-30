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

use derive_more::Display;
use fnv::FnvHasher;
use gazebo::prelude::*;
use internment_tweaks::Equiv;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;

use crate::cells::cell_path::CellPath;
use crate::cells::paths::CellRelativePath;
use crate::cells::CellName;
use crate::cells::CellResolver;
use crate::fs::paths::fmt::quoted_display;
use crate::fs::paths::ForwardRelativePath;
use crate::fs::project::ProjectRelativePathBuf;

/// A 'Package' as defined above.
#[derive(Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub struct Package(Intern<PackageData>);

/// Intern is Copy, so Clone is super cheap
impl Dupe for Package {}

/// The Hash impl for Intern will hash the pointer. That's not stable across runs and its too
/// easy for us to assume that it would be, so use a deterministic hash here.
#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq is still correct.
impl std::hash::Hash for Package {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.0).hash(state)
    }
}

#[derive(Debug, Display, Ord, PartialOrd)]
struct PackageData(CellPath);

#[derive(Hash, Eq, PartialEq)]
struct PackageDataRef<'a> {
    cell: &'a CellName,
    path: &'a CellRelativePath,
}

impl<'a> From<PackageDataRef<'a>> for PackageData {
    fn from(package_data: PackageDataRef<'a>) -> Self {
        PackageData(CellPath::new(
            package_data.cell.clone(),
            package_data.path.to_buf(),
        ))
    }
}

impl PackageData {
    fn as_ref(&self) -> PackageDataRef {
        PackageDataRef {
            cell: self.0.cell(),
            path: self.0.path(),
        }
    }
}

impl PartialEq for PackageData {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for PackageData {}

impl Hash for PackageData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<'a> Equiv<PackageData> for PackageDataRef<'a> {
    fn equivalent(&self, key: &PackageData) -> bool {
        self == &key.as_ref()
    }
}

static INTERNER: StaticInterner<PackageData, FnvHasher> = StaticInterner::new();

impl Package {
    pub fn new(cell: &CellName, path: &CellRelativePath) -> Self {
        Self(INTERNER.intern(PackageDataRef { cell, path }))
    }

    pub fn from_cell_path(path: &CellPath) -> Self {
        Self::new(path.cell(), path.path())
    }

    pub fn cell_name(&self) -> &CellName {
        self.0.0.cell()
    }

    pub fn cell_relative_path(&self) -> &CellRelativePath {
        self.0.0.path()
    }

    pub fn to_cell_path(&self) -> CellPath {
        self.0.0.clone()
    }

    pub fn as_cell_path(&self) -> &CellPath {
        &self.0.0
    }

    pub fn join_unnormalized(&self, path: &ForwardRelativePath) -> Self {
        if path.is_empty() {
            self.dupe()
        } else {
            Package::new(
                self.as_cell_path().cell(),
                &self.as_cell_path().path().join_unnormalized(path),
            )
        }
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
    /// use buck2_core::cells::{CellResolver, CellsConfigParser, CellName};
    /// use buck2_core::fs::project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf};
    /// use buck2_core::fs::paths::{ForwardRelativePathBuf, ForwardRelativePath, AbsPathBuf};
    /// use buck2_core::package::Package;
    /// use gazebo::file;
    /// use std::convert::TryFrom;
    /// use buck2_core::cells::cell_root_path::CellRootPathBuf;
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// let temp = tempfile::tempdir()?;
    /// let fs = ProjectFilesystem::new(
    ///     AbsPathBuf::try_from(temp.into_path())?
    /// );
    /// let cell_config = ForwardRelativePathBuf::unchecked_new("myconfig".into());
    /// let cell_path = ProjectRelativePath::new("my/cell")?;
    ///
    /// file::create_dirs_and_write(
    ///     fs.resolve(&cell_path.join_unnormalized(&cell_config)),
    ///     "mycell=.\n",
    /// )?;
    ///
    /// let cells = CellsConfigParser::parse_cells_from_path(
    ///     CellRootPathBuf::new(cell_path.to_buf()), &fs, &cell_config)?;
    ///
    /// let pkg = Package::new(
    ///     &CellName::unchecked_new("mycell".into()),
    ///     CellRelativePath::unchecked_new("somepkg"),
    /// );
    ///
    /// assert_eq!(
    ///     cells.resolve_package(&pkg)?,
    ///     ProjectRelativePathBuf::unchecked_new("my/cell/somepkg".into()),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve_package(&self, pkg: &Package) -> anyhow::Result<ProjectRelativePathBuf> {
        self.resolve_path(&pkg.0.0)
    }
}

pub mod testing {
    use crate::cells::paths::CellRelativePathBuf;
    use crate::cells::CellName;
    use crate::package::Package;

    pub trait PackageExt {
        fn testing_new(cell: &str, path: &str) -> Self;
    }

    impl PackageExt for Package {
        fn testing_new(cell: &str, path: &str) -> Self {
            Self::new(
                &CellName::unchecked_new(cell.into()),
                &CellRelativePathBuf::unchecked_new(path.into()),
            )
        }
    }
}
