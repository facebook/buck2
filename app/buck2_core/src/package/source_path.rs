/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_util::arc_str::ArcS;
use derive_more::Display;
use dupe::Dupe;

use crate::cells::cell_path::CellPath;
use crate::package::package_relative_path::PackageRelativePath;
use crate::package::PackageLabel;

/// Represents the path of a source artifact.
#[derive(
    Clone,
    Debug,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
#[display("{}", self.as_ref())]
pub struct SourcePath {
    pkg: PackageLabel,
    path: ArcS<PackageRelativePath>,
}

impl SourcePath {
    #[inline]
    pub fn new(pkg: PackageLabel, path: ArcS<PackageRelativePath>) -> Self {
        SourcePath { pkg, path }
    }

    /// This is slow, but OK to use in tests.
    pub fn testing_new(pkg: &str, path: &str) -> Self {
        SourcePath::new(
            PackageLabel::testing_parse(pkg),
            ArcS::from(PackageRelativePath::new(path).unwrap()),
        )
    }

    #[inline]
    pub fn package(&self) -> PackageLabel {
        self.pkg.dupe()
    }

    #[inline]
    pub fn path(&self) -> &PackageRelativePath {
        &self.path
    }

    #[inline]
    pub fn to_cell_path(&self) -> CellPath {
        self.as_ref().to_cell_path()
    }

    #[inline]
    pub fn as_ref(&self) -> SourcePathRef {
        SourcePathRef {
            pkg: self.pkg.dupe(),
            path: &self.path,
        }
    }
}

#[derive(Display, Debug, Eq, Hash, PartialEq, Copy, Clone, Dupe)]
#[display("{}/{}", pkg, path.as_str())]
pub struct SourcePathRef<'a> {
    pkg: PackageLabel,
    path: &'a ArcS<PackageRelativePath>,
}

impl<'a> SourcePathRef<'a> {
    #[inline]
    pub fn new(pkg: PackageLabel, path: &'a ArcS<PackageRelativePath>) -> SourcePathRef<'a> {
        SourcePathRef { pkg, path }
    }

    #[inline]
    pub fn package(&self) -> PackageLabel {
        self.pkg.dupe()
    }

    #[inline]
    pub fn path(&self) -> &PackageRelativePath {
        self.path
    }

    #[inline]
    pub fn to_cell_path(&self) -> CellPath {
        self.pkg
            .as_cell_path()
            .join(self.path.as_forward_rel_path())
    }

    #[inline]
    pub fn to_owned(&self) -> SourcePath {
        SourcePath {
            pkg: self.pkg.dupe(),
            path: self.path.dupe(),
        }
    }
}
