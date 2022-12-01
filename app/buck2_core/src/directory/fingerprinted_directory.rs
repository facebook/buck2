/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::iter::once;

use super::DirectoryEntry;
use super::DirectoryHasher;
use super::FingerprintedOrderedDirectoryWalk;
use super::FingerprintedUnorderedDirectoryWalk;
use super::HasDirectoryDigest;
use super::PathAccumulator;
use crate::fs::paths::file_name::FileName;

pub type FingerprintedDirectoryEntries<'a, L, H> = Box<
    dyn Iterator<
            Item = (
                &'a FileName,
                DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>,
            ),
        > + 'a,
>;

pub trait FingerprintedDirectory<L, H> {
    fn fingerprinted_entries(&self) -> FingerprintedDirectoryEntries<'_, L, H>;

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>>;

    fn fingerprinted_unordered_walk(&self) -> FingerprintedUnorderedDirectoryWalk<'_, L, H> {
        FingerprintedUnorderedDirectoryWalk::new(self)
    }

    fn fingerprinted_ordered_walk(&self) -> FingerprintedOrderedDirectoryWalk<'_, L, H> {
        FingerprintedOrderedDirectoryWalk::new(self)
    }

    fn fingerprint(&self) -> &<H as HasDirectoryDigest>::Digest
    where
        H: HasDirectoryDigest;
}

impl<'a, L, H> fmt::Debug for &'a dyn FingerprintedDirectory<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FingerprintedDirectory")
    }
}
