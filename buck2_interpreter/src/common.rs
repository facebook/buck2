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

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::bzl::ModuleID;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use derive_more::Display;
use gazebo::prelude::*;
use gazebo::variants::UnpackVariants;
use ref_cast::RefCast;
use thiserror::Error;

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

    pub fn starlark_path(&self) -> StarlarkPath {
        match self {
            StarlarkModulePath::LoadFile(l) => StarlarkPath::LoadFile(l),
            StarlarkModulePath::BxlFile(b) => StarlarkPath::BxlFile(b),
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
