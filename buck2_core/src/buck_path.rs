/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::cells::cell_path::CellPath;
use crate::package::package_relative_path::PackageRelativePath;
use crate::package::package_relative_path::PackageRelativePathBuf;
use crate::package::Package;

/// Represents a resolvable path corresponding to some path that is part of a
/// 'Package'. The 'BuckPath' refers to only paths in the repo source, not
/// outputs of a 'Package'.
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
#[display(fmt = "{}/{}", pkg, "path.as_str()")]
pub struct BuckPath {
    pkg: Package,
    path: PackageRelativePathBuf,
}

impl BuckPath {
    pub fn new(pkg: Package, path: PackageRelativePathBuf) -> Self {
        BuckPath { pkg, path }
    }

    pub fn package(&self) -> &Package {
        &self.pkg
    }

    pub fn path(&self) -> &PackageRelativePath {
        &self.path
    }

    pub fn to_cell_path(&self) -> CellPath {
        self.pkg.as_cell_path().join(&self.path)
    }
}
