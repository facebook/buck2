/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use allocative::Allocative;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_util::arc_str::ArcS;
use either::Either;
use static_assertions::assert_eq_size;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, strong_hash::StrongHash)]
pub struct CoercedDirectory {
    pub dir: ArcS<PackageRelativePath>,
    // We can make this type DST, so there would be only one allocation
    // for directory itself and files. But we don't have a lot of directories,
    // so it is not worth the trouble.
    pub files: Box<[ArcS<PackageRelativePath>]>,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, strong_hash::StrongHash)]
pub enum CoercedPath {
    File(ArcS<PackageRelativePath>),
    Directory(Box<CoercedDirectory>),
}

// Avoid changing the size accidentally.
assert_eq_size!(CoercedPath, [usize; 2]);

impl CoercedPath {
    pub fn path(&self) -> &ArcS<PackageRelativePath> {
        match self {
            CoercedPath::File(x) => x,
            CoercedPath::Directory(x) => &x.dir,
        }
    }

    pub fn inputs(&self) -> impl Iterator<Item = &'_ ArcS<PackageRelativePath>> {
        match self {
            CoercedPath::File(x) => Either::Left(iter::once(x)),
            CoercedPath::Directory(x) => Either::Right(x.files.iter()),
        }
    }
}
