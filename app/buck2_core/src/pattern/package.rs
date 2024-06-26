/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

use crate::cells::cell_path::CellPath;
use crate::package::PackageLabel;

#[derive(Clone, Debug, Eq, PartialEq, Allocative)]
pub enum PackagePattern {
    Package(PackageLabel),
    Recursive(CellPath),
}

impl PackagePattern {
    pub fn matches(&self, package: PackageLabel) -> bool {
        match self {
            PackagePattern::Package(pattern) => pattern == &package,
            PackagePattern::Recursive(cell_path) => {
                package.as_cell_path().starts_with(cell_path.as_ref())
            }
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Allocative)]
pub enum PackagePredicate {
    Any,
    AnyOf(Vec<PackagePattern>),
}

impl PackagePredicate {
    pub fn matches(&self, package: PackageLabel) -> bool {
        match self {
            PackagePredicate::Any => true,
            PackagePredicate::AnyOf(patterns) => {
                patterns.iter().any(|pattern| pattern.matches(package))
            }
        }
    }
}
