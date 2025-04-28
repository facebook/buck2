/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::directory_iterator::DirectoryIterator;
use crate::directory::directory_ref::DirectoryRef;
use crate::directory::entry::DirectoryEntry;
use crate::directory::walk::OrderedDirectoryWalk;
use crate::directory::walk::UnorderedDirectoryWalk;

pub type DirectoryEntries<'a, L, H> =
    Box<dyn Iterator<Item = (&'a FileName, DirectoryEntry<&'a dyn Directory<L, H>, &'a L>)> + 'a>;

/// A Directory that may or may not be fingerprinted. This means it only exposes the common
/// denominator of operations available on such Directories, which is to access entries in them.
pub trait Directory<L, H> {
    type DirectoryRef<'a>: DirectoryRef<'a, Leaf = L, DirectoryDigest = H>
    where
        Self: Sized + 'a,
        L: 'a;

    fn as_ref<'a>(&'a self) -> Self::DirectoryRef<'a>
    where
        Self: Sized + 'a;

    fn unordered_walk<'a>(&'a self) -> UnorderedDirectoryWalk<'a, Self::DirectoryRef<'a>>
    where
        Self: Sized,
    {
        UnorderedDirectoryWalk::new(self.as_ref())
    }

    fn unordered_walk_leaves<'a>(&'a self) -> impl DirectoryIterator<Item = &'a L>
    where
        Self: Sized,
        H: 'a,
        L: 'a,
    {
        self.unordered_walk().leaves()
    }

    fn ordered_walk<'a>(&'a self) -> OrderedDirectoryWalk<'a, Self::DirectoryRef<'a>>
    where
        Self: Sized,
    {
        OrderedDirectoryWalk::new(self.as_ref())
    }

    fn ordered_walk_leaves<'a>(&'a self) -> impl DirectoryIterator<Item = &'a L>
    where
        Self: Sized,
        H: 'a,
        L: 'a,
    {
        self.ordered_walk().leaves()
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
        H: DirectoryDigest;
}

impl<'a, L, H> fmt::Debug for &'a dyn Directory<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Directory")
    }
}
