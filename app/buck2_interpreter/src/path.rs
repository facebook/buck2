/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::fs::paths::file_name::FileName;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use starlark::collections::Equivalent;
use thiserror::Error;

use crate::file_type::StarlarkFileType;

/// Path of a `bxl` file for `bxl` commands
#[derive(
    Clone,
    Hash,
    Eq,
    PartialEq,
    Debug,
    derive_more::Display,
    Ord,
    PartialOrd,
    Allocative
)]
#[display(fmt = "{}", path)]
pub struct BxlFilePath {
    /// The path of this bxl file, including the `bxl` extension
    path: CellPath,
}

#[derive(Debug, Error)]
#[error("Expected a cell path to a `.bxl` file, but got `{0}`")]
struct BxlPathError(CellPath);

impl BxlFilePath {
    pub fn new(path: CellPath) -> anyhow::Result<Self> {
        let err = || BxlPathError(path.clone());

        if path.path().file_name().ok_or_else(err)?.extension() != Some("bxl") {
            return Err(anyhow::anyhow!(err()));
        }

        Ok(Self::unverified_new(path))
    }

    fn unverified_new(path: CellPath) -> Self {
        BxlFilePath { path }
    }

    pub fn testing_new(cell: &str, cell_relative_path: &str) -> Self {
        let path = CellPath::new(
            CellName::testing_new(cell),
            CellRelativePathBuf::try_from(cell_relative_path.to_owned()).unwrap(),
        );
        Self::unverified_new(path)
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        BuildFileCell::new(self.cell())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative, derive_more::Display)]
#[display(fmt = "{}", path)]
pub struct PackageFilePath {
    /// Including `/PACKAGE`.
    path: CellPath,
}

impl PackageFilePath {
    pub const PACKAGE_FILE_NAME: &'static FileName = FileName::unchecked_new("PACKAGE");

    /// Create for directory containing `PACKAGE` file.
    pub fn for_dir(path: CellPathRef) -> PackageFilePath {
        PackageFilePath {
            path: path.join(Self::PACKAGE_FILE_NAME),
        }
    }

    pub fn cell(&self) -> CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        BuildFileCell::new(self.cell())
    }

    /// Directory containing this `PACKAGE` file.
    pub fn dir(&self) -> CellPathRef {
        self.path
            .parent()
            .expect("constructor verifies that path is not root")
    }

    pub fn parent_package_file(&self) -> Option<PackageFilePath> {
        self.dir().parent().map(PackageFilePath::for_dir)
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }
}

/// Path to file containing starlark that can be evaluated by the interpreter.
#[derive(Display, Clone, Copy, Dupe, Debug, UnpackVariants)]
#[display(fmt = "{}", self.id())]
pub enum StarlarkPath<'a> {
    /// a build file
    BuildFile(&'a BuildFilePath),
    /// a `PACKAGE` file
    PackageFile(&'a PackageFilePath),
    /// a file to be imported
    LoadFile(&'a ImportPath),
    /// a bxl file to be evaluated
    BxlFile(&'a BxlFilePath),
}

impl<'a> StarlarkPath<'a> {
    pub fn cell(&self) -> CellName {
        match self {
            StarlarkPath::BuildFile(b) => b.cell(),
            StarlarkPath::PackageFile(p) => p.cell(),
            StarlarkPath::LoadFile(l) => l.cell(),
            StarlarkPath::BxlFile(b) => b.cell(),
        }
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        match self {
            StarlarkPath::BuildFile(b) => b.build_file_cell(),
            StarlarkPath::PackageFile(p) => p.build_file_cell(),
            StarlarkPath::LoadFile(l) => l.build_file_cell(),
            StarlarkPath::BxlFile(b) => b.build_file_cell(),
        }
    }

    pub fn path(&self) -> Cow<'a, CellPath> {
        match self {
            StarlarkPath::BuildFile(b) => Cow::Owned(b.path()),
            StarlarkPath::PackageFile(p) => Cow::Borrowed(p.path()),
            StarlarkPath::LoadFile(l) => Cow::Borrowed(l.path()),
            StarlarkPath::BxlFile(b) => Cow::Borrowed(b.path()),
        }
    }

    pub fn file_type(&self) -> StarlarkFileType {
        match self {
            StarlarkPath::BuildFile(_) => StarlarkFileType::Buck,
            StarlarkPath::PackageFile(_) => StarlarkFileType::Package,
            StarlarkPath::LoadFile(_) => StarlarkFileType::Bzl,
            StarlarkPath::BxlFile(_) => StarlarkFileType::Bxl,
        }
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{}", self.borrow())]
pub enum OwnedStarlarkPath {
    /// a build file
    BuildFile(BuildFilePath),
    /// a package file
    PackageFile(PackageFilePath),
    /// a file to be imported
    LoadFile(ImportPath),
    /// a bxl file to be evaluated
    BxlFile(BxlFilePath),
}

impl OwnedStarlarkPath {
    pub fn new(path: StarlarkPath<'_>) -> Self {
        match path {
            StarlarkPath::BuildFile(p) => Self::BuildFile(p.clone()),
            StarlarkPath::PackageFile(p) => Self::PackageFile(p.clone()),
            StarlarkPath::LoadFile(p) => Self::LoadFile(p.clone()),
            StarlarkPath::BxlFile(p) => Self::BxlFile(p.clone()),
        }
    }

    pub fn borrow(&self) -> StarlarkPath<'_> {
        match self {
            OwnedStarlarkPath::BuildFile(p) => StarlarkPath::BuildFile(p),
            OwnedStarlarkPath::PackageFile(p) => StarlarkPath::PackageFile(p),
            OwnedStarlarkPath::LoadFile(p) => StarlarkPath::LoadFile(p),
            OwnedStarlarkPath::BxlFile(p) => StarlarkPath::BxlFile(p),
        }
    }
}

impl<'a> From<StarlarkModulePath<'a>> for StarlarkPath<'a> {
    fn from(s: StarlarkModulePath<'a>) -> Self {
        match s {
            StarlarkModulePath::LoadFile(p) => StarlarkPath::LoadFile(p),
            StarlarkModulePath::BxlFile(p) => StarlarkPath::BxlFile(p),
        }
    }
}

/// Path to file containing starlark that can be evaluated as a loaded module by the interpreter.
#[derive(Display, Clone, Copy, Dupe, Debug, UnpackVariants, Hash, Eq, PartialEq)]
#[display(fmt = "{}", self.id())]
pub enum StarlarkModulePath<'a> {
    /// a file to be imported
    LoadFile(&'a ImportPath),
    /// a bxl file to be evaluated
    BxlFile(&'a BxlFilePath),
}

impl Equivalent<OwnedStarlarkModulePath> for StarlarkModulePath<'_> {
    fn equivalent(&self, key: &OwnedStarlarkModulePath) -> bool {
        *self == key.borrow()
    }
}

impl<'a> StarlarkModulePath<'a> {
    pub fn cell(&self) -> CellName {
        match self {
            StarlarkModulePath::LoadFile(l) => l.cell(),
            StarlarkModulePath::BxlFile(b) => b.cell(),
        }
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        match self {
            StarlarkModulePath::LoadFile(l) => l.build_file_cell(),
            StarlarkModulePath::BxlFile(b) => b.build_file_cell(),
        }
    }

    pub fn path(&self) -> &CellPath {
        match self {
            StarlarkModulePath::LoadFile(l) => l.path(),
            StarlarkModulePath::BxlFile(b) => b.path(),
        }
    }

    pub fn starlark_path(&self) -> StarlarkPath {
        match self {
            StarlarkModulePath::LoadFile(l) => StarlarkPath::LoadFile(l),
            StarlarkModulePath::BxlFile(b) => StarlarkPath::BxlFile(b),
        }
    }

    pub fn to_owned(&self) -> OwnedStarlarkModulePath {
        OwnedStarlarkModulePath::new(*self)
    }
}

#[derive(Clone, Display, Debug, Eq, PartialEq, Allocative)]
#[display(fmt = "{}", self.borrow())]
pub enum OwnedStarlarkModulePath {
    LoadFile(ImportPath),
    BxlFile(BxlFilePath),
}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for OwnedStarlarkModulePath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.borrow().hash(state)
    }
}

impl OwnedStarlarkModulePath {
    pub fn new(path: StarlarkModulePath<'_>) -> Self {
        match path {
            StarlarkModulePath::LoadFile(p) => Self::LoadFile(p.clone()),
            StarlarkModulePath::BxlFile(p) => Self::BxlFile(p.clone()),
        }
    }

    pub fn borrow(&self) -> StarlarkModulePath<'_> {
        match self {
            OwnedStarlarkModulePath::LoadFile(p) => StarlarkModulePath::LoadFile(p),
            OwnedStarlarkModulePath::BxlFile(p) => StarlarkModulePath::BxlFile(p),
        }
    }

    pub fn path(&self) -> CellPathRef<'_> {
        match self {
            OwnedStarlarkModulePath::LoadFile(p) => p.path().as_ref(),
            OwnedStarlarkModulePath::BxlFile(p) => p.path().as_ref(),
        }
    }

    pub fn into_starlark_path(self) -> OwnedStarlarkPath {
        match self {
            OwnedStarlarkModulePath::LoadFile(l) => OwnedStarlarkPath::LoadFile(l),
            OwnedStarlarkModulePath::BxlFile(b) => OwnedStarlarkPath::BxlFile(b),
        }
    }
}
