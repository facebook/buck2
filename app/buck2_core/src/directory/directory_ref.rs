/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::directory::builder::DirectoryBuilder;
use crate::directory::directory::Directory;
use crate::directory::directory::DirectoryEntries;
use crate::directory::directory_hasher::DirectoryDigest;
use crate::directory::entry::DirectoryEntry;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;
use crate::directory::fingerprinted_directory::FingerprintedDirectoryEntries;
use crate::fs::paths::file_name::FileName;

pub trait DirectoryRef<'a>: Copy + 'a + Sized {
    type Leaf;
    type DirectoryDigest;

    type Entries: Iterator<Item = (&'a FileName, DirectoryEntry<Self, &'a Self::Leaf>)>;

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

impl<'a, L, H> DirectoryRef<'a> for &'a dyn Directory<L, H> {
    type Leaf = L;
    type DirectoryDigest = H;

    type Entries = DirectoryEntries<'a, L, H>;

    fn entries(self) -> Self::Entries {
        self.entries()
    }

    fn as_dyn(self) -> &'a dyn Directory<Self::Leaf, Self::DirectoryDigest> {
        self
    }
}

impl<'a, L, H> DirectoryRef<'a> for &'a dyn FingerprintedDirectory<L, H> {
    type Leaf = L;
    type DirectoryDigest = H;

    type Entries = FingerprintedDirectoryEntries<'a, L, H>;

    fn entries(self) -> Self::Entries {
        self.fingerprinted_entries()
    }

    fn as_dyn(self) -> &'a dyn Directory<Self::Leaf, Self::DirectoryDigest> {
        self.as_directory()
    }
}

impl<'a, L, H> FingerprintedDirectoryRef<'a> for &'a dyn FingerprintedDirectory<L, H> {
    fn as_fingerprinted_dyn(self) -> &'a dyn FingerprintedDirectory<L, H> {
        self
    }
}
