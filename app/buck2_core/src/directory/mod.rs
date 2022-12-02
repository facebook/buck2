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

pub use builder::DirectoryBuilder;
pub use builder::DirectoryInsertError;
pub use builder::DirectoryMergeError;
pub use builder::DirectoryMkdirError;
pub use dashmap_directory_interner::DashMapDirectoryInterner;
pub use directory::Directory;
pub use directory::DirectoryEntries;
pub use directory_data::DirectoryData;
pub use directory_hasher::DirectoryHasher;
pub use directory_hasher::HasDirectoryDigest;
pub use directory_iterator::DirectoryIterator;
pub use directory_iterator::DirectoryIteratorPathAccessor;
pub use directory_iterator::DirectoryIteratorPathStack;
pub use directory_iterator::DirectoryIteratorWithPaths;
pub use directory_iterator::DirectoryIteratorWithoutPaths;
pub use directory_selector::DirectorySearchError;
pub use directory_selector::DirectorySelector;
pub use directory_selector::FingerprintedOrderedDirectorySearch;
pub use directory_selector::FingerprintedUnorderedDirectorySearch;
pub use directory_selector::OrderedDirectorySearch;
pub use directory_selector::UnorderedDirectorySearch;
use entries::FingerprintedOrderedDirectoryEntries;
use entries::OrderedDirectoryEntries;
pub use entry::DirectoryEntry;
pub use exclusive_directory::ExclusiveDirectory;
pub use find::find;
pub use find::find_fingerprinted;
pub use find::find_prefix;
pub use find::find_prefix_fingerprinted;
pub use find::DirectoryFindError;
pub use fingerprinted_directory::FingerprintedDirectory;
pub use fingerprinted_directory::FingerprintedDirectoryEntries;
pub use immutable_directory::ImmutableDirectory;
pub use no_hasher::NoHasher;
pub use path_accumulator::PathAccumulator;
pub use shared_directory::SharedDirectory;
pub use shared_directory::SharedDirectoryData;
pub use shared_directory::SharedDirectoryInner;
pub use walk::fingerprinted_ordered_entry_walk;
pub use walk::fingerprinted_unordered_entry_walk;
pub use walk::ordered_entry_walk;
pub use walk::unordered_entry_walk;
pub use walk::DirectoryEntryWalk;
pub use walk::FingerprintedOrderedDirectoryWalk;
pub use walk::FingerprintedUnorderedDirectoryWalk;
pub use walk::OrderedDirectoryWalk;
pub use walk::UnorderedDirectoryWalk;

use self::macros::impl_fingerprinted_directory;
