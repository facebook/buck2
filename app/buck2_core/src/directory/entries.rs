/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use super::Directory;
use super::DirectoryEntries;
use super::DirectoryEntry;
use super::FingerprintedDirectory;
use super::FingerprintedDirectoryEntries;
use crate::fs::paths::file_name::FileName;

macro_rules! impl_ordered_entries {
    ($ordered_entries_ty: ident, $dir_ty: ident, $from_ty: ident,) => {
        /// A wrapper struct that allows an ordered traversal of directory entries.
        pub struct $ordered_entries_ty<'a, L, H> {
            entries: Vec<(&'a FileName, DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>)>,
        }

        impl<'a, L, H> From<$from_ty<'a, L, H>> for $ordered_entries_ty<'a, L, H> {
            fn from(entries: $from_ty<'a, L, H>) -> Self {
                let mut entries = entries.collect::<Vec<_>>();
                entries.sort_by(|(name1, _), (name2, _)| name2.cmp(name1));
                Self { entries }
            }
        }

        impl<'a, L, H> Iterator for $ordered_entries_ty<'a, L, H> {
            type Item = (&'a FileName, DirectoryEntry<&'a dyn $dir_ty<L, H>, &'a L>);

            fn next(&mut self) -> Option<Self::Item> {
                self.entries.pop()
            }
        }
    };
}

impl_ordered_entries!(
    FingerprintedOrderedDirectoryEntries,
    FingerprintedDirectory,
    FingerprintedDirectoryEntries,
);

impl_ordered_entries!(OrderedDirectoryEntries, Directory, DirectoryEntries,);
