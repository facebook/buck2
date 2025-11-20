/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![cfg(test)]

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_fs::paths::file_name::FileName;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use derive_more::Display;
use dupe::Dupe;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::directory_hasher::DirectoryDigester;
use crate::directory::directory_hasher::NoDigest;
use crate::directory::directory_ref::FingerprintedDirectoryRef;
use crate::directory::entry::DirectoryEntry;

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub(crate) struct NopEntry;

pub(crate) struct TestHasher;

#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative, Display)]
pub(crate) struct TestDigest(u64);

impl DirectoryDigest for TestDigest {}

impl DirectoryDigester<NopEntry, TestDigest> for TestHasher {
    fn hash_entries<'a, D, I>(&self, entries: I) -> TestDigest
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<D, &'a NopEntry>)>,
        D: FingerprintedDirectoryRef<'a, Leaf = NopEntry, DirectoryDigest = TestDigest>,
    {
        let mut hasher = DefaultHasher::new();

        let mut entries = entries
            .into_iter()
            .map(|(name, entry)| {
                let entry = entry.map_dir(|d| d.as_fingerprinted_dyn().fingerprint());
                (name, entry)
            })
            .collect::<Vec<_>>();
        entries.sort_by_key(|(name, _)| *name);

        entries.hash(&mut hasher);
        TestDigest(hasher.finish())
    }

    fn leaf_size(&self, _leaf: &NopEntry) -> u64 {
        1
    }
}

pub(crate) type TestDirectoryBuilder = DirectoryBuilder<NopEntry, TestDigest>;
pub(crate) type NoHasherDirectoryBuilder = DirectoryBuilder<NopEntry, NoDigest>;

pub(crate) fn path(s: &str) -> &ForwardRelativePath {
    ForwardRelativePath::unchecked_new(s)
}
