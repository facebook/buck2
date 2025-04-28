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

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bxl::BxlFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::name::CellName;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;

use crate::file_type::StarlarkFileType;
use crate::paths::module::StarlarkModulePath;
use crate::paths::package::PackageFilePath;

/// Path to file containing starlark that can be evaluated by the interpreter.
#[derive(Display, Clone, Copy, Dupe, Debug, UnpackVariants, PartialEq, Eq, Hash)]
#[display("{}", self.id())]
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
#[display("{}", self.borrow())]
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
