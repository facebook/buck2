/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::buck_path::BuckPath;
use either::Either;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub enum CoercedPath {
    File(BuckPath),
    Directory(BuckPath, Vec<BuckPath>),
}

impl CoercedPath {
    pub fn path(&self) -> &BuckPath {
        match self {
            CoercedPath::File(x) => x,
            CoercedPath::Directory(x, _) => x,
        }
    }

    pub fn inputs(&self) -> impl Iterator<Item = &BuckPath> {
        match self {
            CoercedPath::File(x) => Either::Left(std::iter::once(x)),
            CoercedPath::Directory(_, xs) => Either::Right(xs.iter()),
        }
    }
}
