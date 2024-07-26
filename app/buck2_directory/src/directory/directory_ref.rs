/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::directory::Directory;
use crate::directory::entry::DirectoryEntry;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;

pub trait DirectoryRef<'a>: Copy + 'a + Sized {
    type Leaf;
    type DirectoryDigest;

    type Entries: Iterator<Item = (&'a FileName, DirectoryEntry<Self, &'a Self::Leaf>)>;

    fn get(self, name: &FileName) -> Option<DirectoryEntry<Self, &'a Self::Leaf>>;

    fn entries(self) -> Self::Entries;

    fn as_dyn(self) -> &'a dyn Directory<Self::Leaf, Self::DirectoryDigest>;

    fn to_builder(self) -> DirectoryBuilder<Self::Leaf, Self::DirectoryDigest>
    where
        Self::DirectoryDigest: DirectoryDigest,
        Self::Leaf: Clone,
    {
        self.as_dyn().to_builder()
    }
}

pub trait FingerprintedDirectoryRef<'a>: DirectoryRef<'a> {
    fn as_fingerprinted_dyn(
        self,
    ) -> &'a dyn FingerprintedDirectory<Self::Leaf, Self::DirectoryDigest>;
}
