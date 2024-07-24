/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::directory::directory::Directory;
use crate::directory::directory_hasher::DirectoryDigest;
use crate::directory::directory_iterator::DirectoryIterator;
use crate::directory::entry::DirectoryEntry;
use crate::directory::walk::FingerprintedOrderedDirectoryWalk;
use crate::directory::walk::FingerprintedUnorderedDirectoryWalk;
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
    fn as_directory(&self) -> &dyn Directory<L, H>;

    fn fingerprinted_entries(&self) -> FingerprintedDirectoryEntries<'_, L, H>;

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>>;

    fn fingerprinted_unordered_walk(&self) -> FingerprintedUnorderedDirectoryWalk<'_, L, H>
    where
        Self: Sized,
    {
        FingerprintedUnorderedDirectoryWalk::new(self)
    }

    fn fingerprinted_unordered_walk_leaves<'a>(&'a self) -> impl DirectoryIterator<Item = &'a L>
    where
        Self: Sized,
        L: 'a,
        H: 'a,
    {
        self.fingerprinted_unordered_walk()
            .filter_map(|entry| match entry {
                DirectoryEntry::Leaf(leaf) => Some(leaf),
                DirectoryEntry::Dir(_) => None,
            })
    }

    fn fingerprinted_ordered_walk(&self) -> FingerprintedOrderedDirectoryWalk<'_, L, H>
    where
        Self: Sized,
    {
        FingerprintedOrderedDirectoryWalk::new(self)
    }

    fn fingerprinted_ordered_walk_leaves<'a>(&'a self) -> impl DirectoryIterator<Item = &'a L>
    where
        Self: Sized,
        L: 'a,
        H: 'a,
    {
        self.fingerprinted_ordered_walk()
            .filter_map(|entry| match entry {
                DirectoryEntry::Leaf(leaf) => Some(leaf),
                DirectoryEntry::Dir(_) => None,
            })
    }

    fn fingerprint(&self) -> &H
    where
        H: DirectoryDigest;
}

impl<'a, L, H> fmt::Debug for &'a dyn FingerprintedDirectory<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FingerprintedDirectory")
    }
}
