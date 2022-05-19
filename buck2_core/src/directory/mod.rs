/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![allow(clippy::module_inception)]
#![allow(unused)]

use std::sync::Weak;

mod builder;
mod dashmap_directory_interner;
mod directory;
mod directory_data;
mod directory_hasher;
mod directory_iterator;
mod directory_selector;
mod entries;
mod entry;
mod exclusive_directory;
mod find;
mod fingerprinted_directory;
mod immutable_directory;
mod macros;
mod no_hasher;
mod path_accumulator;
mod shared_directory;
mod test;
mod walk;

pub use builder::{
    DirectoryBuilder, DirectoryInsertError, DirectoryMergeError, DirectoryMkdirError,
};
pub use dashmap_directory_interner::DashMapDirectoryInterner;
pub use directory::{Directory, DirectoryEntries};
pub use directory_data::DirectoryData;
pub use directory_hasher::{DirectoryHasher, HasDirectoryDigest};
pub use directory_iterator::{
    DirectoryIterator, DirectoryIteratorPathAccessor, DirectoryIteratorPathStack,
    DirectoryIteratorWithPaths, DirectoryIteratorWithoutPaths,
};
pub use directory_selector::{
    DirectorySearchError, DirectorySelector, FingerprintedOrderedDirectorySearch,
    FingerprintedUnorderedDirectorySearch, OrderedDirectorySearch, UnorderedDirectorySearch,
};
use entries::{FingerprintedOrderedDirectoryEntries, OrderedDirectoryEntries};
pub use entry::DirectoryEntry;
pub use exclusive_directory::ExclusiveDirectory;
pub use find::{find, find_fingerprinted, DirectoryFindError};
pub use fingerprinted_directory::{FingerprintedDirectory, FingerprintedDirectoryEntries};
pub use immutable_directory::ImmutableDirectory;
pub use no_hasher::NoHasher;
pub use path_accumulator::PathAccumulator;
pub use shared_directory::{SharedDirectory, SharedDirectoryData, SharedDirectoryInner};
pub use walk::{
    fingerprinted_ordered_entry_walk, fingerprinted_unordered_entry_walk, ordered_entry_walk,
    unordered_entry_walk, DirectoryEntryWalk, FingerprintedOrderedDirectoryWalk,
    FingerprintedUnorderedDirectoryWalk, OrderedDirectoryWalk, UnorderedDirectoryWalk,
};

use self::macros::impl_fingerprinted_directory;
