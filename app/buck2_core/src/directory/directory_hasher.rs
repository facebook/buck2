/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use gazebo::prelude::*;

use super::DirectoryEntry;
use super::FingerprintedDirectory;
use crate::fs::paths::file_name::FileName;

pub trait HasDirectoryDigest {
    type Digest: Allocative + PartialEq + Eq + Hash + Clone + Dupe + Debug + Display;
}

pub trait DirectoryHasher<L>: HasDirectoryDigest {
    fn hash_entries<'a, D, I>(entries: I) -> <Self as HasDirectoryDigest>::Digest
    where
        I: Iterator<Item = (&'a FileName, DirectoryEntry<&'a D, &'a L>)>,
        D: FingerprintedDirectory<L, Self> + 'a,
        L: 'a,
        Self: Sized;
}
