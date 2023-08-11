/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use starlark::collections::Equivalent;

use crate::paths::bxl::BxlFilePath;
use crate::paths::path::OwnedStarlarkPath;
use crate::paths::path::StarlarkPath;

/// Path to file containing starlark that can be evaluated as a loaded module by the interpreter.
#[derive(
    derive_more::Display,
    Clone,
    Copy,
    Dupe,
    Debug,
    UnpackVariants,
    Hash,
    Eq,
    PartialEq
)]
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

#[derive(Clone, derive_more::Display, Debug, Eq, PartialEq, Allocative)]
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
