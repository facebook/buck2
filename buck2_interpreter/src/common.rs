/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;

use buck2_core::cells::paths::CellPath;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use buck2_core::fs::paths::FileName;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::package::Package;
use derive_more::Display;
use gazebo::prelude::*;
use gazebo::variants::UnpackVariants;
use ref_cast::RefCast;
use thiserror::Error;

/// The starlark interpreter expects imports
/// to be identified by a String and requires using this id in some cases.
/// The id will contain both the package and filename. For a cross-cell load,
/// it will include an `@cell_name` suffix to indicate the top-level cell
/// being loaded into.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct ModuleID(pub String);

impl Display for ModuleID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl Borrow<str> for ModuleID {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl ModuleID {
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Used to hold the cell name for the top-level build file being interpreted.
/// There's a subtlety where that doesn't necessarily match the cell of the file
/// being interpreted. This gets its own type because its easy to get wrong.
#[derive(Clone, Hash, Eq, PartialEq, Debug, derive_more::Display, RefCast)]
#[display(fmt = "{}", .0)]
#[repr(C)]
pub struct BuildFileCell(CellName);

impl BuildFileCell {
    pub fn new(name: CellName) -> Self {
        Self(name)
    }

    pub fn name(&self) -> &CellName {
        &self.0
    }
}

/// Path of a build file (e.g. `BUCK`) only. (`bzl` files are not included).
#[derive(Clone, Hash, Eq, PartialEq, Debug, derive_more::Display)]
#[display(fmt = "{}", id)]
pub struct BuildFilePath {
    /// The package of this build file
    package: Package,
    /// The build file's filename (which can be configured). i.e. `BUCK`
    filename: FileNameBuf,
    /// A ModuleID for the import.
    id: ModuleID,
}

impl BuildFilePath {
    pub fn new(package: Package, filename: FileNameBuf) -> Self {
        let id = ModuleID(format!("{}:{}", package, filename));
        Self {
            package,
            filename,
            id,
        }
    }

    pub fn unchecked_new(cell: &str, package: &str, filename: &str) -> Self {
        let package = Package::new(
            &CellName::unchecked_new(cell.to_owned()),
            &CellRelativePathBuf::unchecked_new(package.to_owned()),
        );
        let filename = FileNameBuf::unchecked_new(filename.to_owned());
        Self::new(package, filename)
    }

    pub fn cell(&self) -> &CellName {
        self.package.cell_name()
    }

    pub fn package(&self) -> &Package {
        &self.package
    }

    pub fn path(&self) -> CellPath {
        self.package
            .as_cell_path()
            .join_unnormalized(&self.filename)
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
        BuildFileCell::ref_cast(self.cell())
    }

    pub fn filename(&self) -> &FileName {
        &self.filename
    }

    pub fn id(&self) -> &ModuleID {
        &self.id
    }
}

/// Path of a `.bzl` file.
#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct ImportPath {
    /// The path to the import as a 'CellPath', which contains the cell
    /// information and the cell relative path to the bzl file itself, including the bzl suffix
    path: CellPath,
    /// The cell of the top-level build module that this is being loaded
    /// (perhaps transitively) into.
    build_file_cell: BuildFileCell,
    /// A ModuleID for the import.
    id: ModuleID,
}

impl ImportPath {
    pub fn new(path: CellPath, build_file_cell: BuildFileCell) -> anyhow::Result<Self> {
        Ok(Self::unverified_new(path, build_file_cell))
    }

    pub fn unverified_new(path: CellPath, build_file_cell: BuildFileCell) -> Self {
        let id = ModuleID(if &build_file_cell.0 == path.cell() {
            format!("{}", path)
        } else {
            format!("{}@{}", path, build_file_cell.0)
        });
        Self {
            path,
            build_file_cell,
            id,
        }
    }

    pub fn unchecked_new(cell: &str, cell_relative_path: &str, filename: &str) -> Self {
        let cell = cell.to_owned();
        Self::unchecked_new_cross_cell(&cell, cell_relative_path, filename, &cell)
    }

    pub fn unchecked_new_cross_cell(
        cell: &str,
        cell_relative_path: &str,
        filename: &str,
        build_file_cell: &str,
    ) -> Self {
        let cell_path = CellPath::new(
            CellName::unchecked_new(cell.to_owned()),
            CellRelativePath::unchecked_new(cell_relative_path)
                .join_unnormalized(FileName::unchecked_new(filename)),
        );
        Self::unverified_new(
            cell_path,
            BuildFileCell(CellName::unchecked_new(build_file_cell.to_owned())),
        )
    }

    pub fn cell(&self) -> &CellName {
        self.path.cell()
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
        &self.build_file_cell
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn id(&self) -> &ModuleID {
        &self.id
    }
}

impl Display for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

/// Path of a `bxl` file for `bxl` commands
#[derive(
    Clone,
    Hash,
    Eq,
    PartialEq,
    Debug,
    derive_more::Display,
    Ord,
    PartialOrd
)]
#[display(fmt = "{}", id)]
pub struct BxlFilePath {
    /// The path of this bxl file, including the `bxl` extension
    path: CellPath,
    /// A ModuleID for the import.
    id: ModuleID,
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

    pub fn unverified_new(path: CellPath) -> Self {
        let id = ModuleID(format!("{}", path));
        Self { path, id }
    }

    pub fn unchecked_new(cell: &str, cell_relative_path: &str) -> Self {
        let path = CellPath::new(
            CellName::unchecked_new(cell.to_owned()),
            CellRelativePathBuf::unchecked_new(cell_relative_path.to_owned()),
        );
        Self::unverified_new(path)
    }

    pub fn cell(&self) -> &CellName {
        self.path.cell()
    }

    pub fn path(&self) -> &CellPath {
        &self.path
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
        BuildFileCell::ref_cast(self.cell())
    }

    pub fn id(&self) -> &ModuleID {
        &self.id
    }
}

/// Path to file containing starlark that can be evaluated by the interpreter.
#[derive(Display, Clone, Copy, Dupe, Debug, UnpackVariants)]
#[display(fmt = "{}", self.id())]
pub enum StarlarkPath<'a> {
    /// a build file
    BuildFile(&'a BuildFilePath),
    /// a file to be imported
    LoadFile(&'a ImportPath),
    /// a bxl file to be evaluated
    BxlFile(&'a BxlFilePath),
}

impl<'a> StarlarkPath<'a> {
    pub fn cell(&self) -> &CellName {
        match self {
            StarlarkPath::BuildFile(b) => b.cell(),
            StarlarkPath::LoadFile(l) => l.cell(),
            StarlarkPath::BxlFile(b) => b.cell(),
        }
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
        match self {
            StarlarkPath::BuildFile(b) => b.build_file_cell(),
            StarlarkPath::LoadFile(l) => l.build_file_cell(),
            StarlarkPath::BxlFile(b) => b.build_file_cell(),
        }
    }

    pub fn path(&self) -> Cow<CellPath> {
        match self {
            StarlarkPath::BuildFile(b) => Cow::Owned(b.path()),
            StarlarkPath::LoadFile(l) => Cow::Borrowed(l.path()),
            StarlarkPath::BxlFile(b) => Cow::Borrowed(b.path()),
        }
    }

    pub fn id(&self) -> &ModuleID {
        match self {
            StarlarkPath::BuildFile(b) => b.id(),
            StarlarkPath::LoadFile(l) => l.id(),
            StarlarkPath::BxlFile(b) => b.id(),
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
#[derive(Display, Clone, Copy, Dupe, Debug, UnpackVariants)]
#[display(fmt = "{}", self.id())]
pub enum StarlarkModulePath<'a> {
    /// a file to be imported
    LoadFile(&'a ImportPath),
    /// a bxl file to be evaluated
    BxlFile(&'a BxlFilePath),
}

impl<'a> StarlarkModulePath<'a> {
    pub fn cell(&self) -> &CellName {
        match self {
            StarlarkModulePath::LoadFile(l) => l.cell(),
            StarlarkModulePath::BxlFile(b) => b.cell(),
        }
    }

    pub fn build_file_cell(&self) -> &BuildFileCell {
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

    pub fn id(&self) -> &ModuleID {
        match self {
            StarlarkModulePath::LoadFile(l) => l.id(),
            StarlarkModulePath::BxlFile(b) => b.id(),
        }
    }
}

#[derive(Clone, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{}", self.borrow())]
pub enum OwnedStarlarkModulePath {
    LoadFile(ImportPath),
    BxlFile(BxlFilePath),
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
}
