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

use super::DirectoryBuilder;
use super::DirectoryEntry;
use super::HasDirectoryDigest;
use super::OrderedDirectoryWalk;
use super::PathAccumulator;
use super::UnorderedDirectoryWalk;
use crate::fs::paths::FileName;

pub type DirectoryEntries<'a, L, H> =
    Box<dyn Iterator<Item = (&'a FileName, DirectoryEntry<&'a dyn Directory<L, H>, &'a L>)> + 'a>;

/// A Directory that may or may not be fingerprinted. This means it only exposes the common
/// denominator of operations available on such Directories, which is to access entries in them.
pub trait Directory<L, H> {
    fn entries(&self) -> DirectoryEntries<'_, L, H>;

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn Directory<L, H>, &'a L>>;

    fn unordered_walk<'a>(&'a self) -> UnorderedDirectoryWalk<'a, L, H> {
        UnorderedDirectoryWalk::new(self)
    }

    fn ordered_walk<'a>(&'a self) -> OrderedDirectoryWalk<'a, L, H> {
        OrderedDirectoryWalk::new(self)
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
        H: HasDirectoryDigest;
}

impl<'a, L, H> fmt::Debug for &'a dyn Directory<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Directory")
    }
}
