/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dashmap::mapref::entry::Entry;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;

use super::impl_fingerprinted_directory;
use super::DashMapDirectoryInterner;
use super::Directory;
use super::DirectoryBuilder;
use super::DirectoryData;
use super::DirectoryEntries;
use super::DirectoryEntry;
use super::DirectoryHasher;
use super::FingerprintedDirectory;
use super::FingerprintedDirectoryEntries;
use super::HasDirectoryDigest;
use super::ImmutableDirectory;
use super::SharedDirectory;
use crate::fs::paths::file_name::FileName;
use crate::fs::paths::file_name::FileNameBuf;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
#[display(fmt = "{}", "self.data")]
pub struct ExclusiveDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    pub(super) data: DirectoryData<ImmutableDirectory<L, H>, L, H>,
}

impl<L, H> ExclusiveDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    pub fn shared(self, interner: &DashMapDirectoryInterner<L, H>) -> SharedDirectory<L, H> {
        if let Some(shared) = interner.get(self.fingerprint()) {
            return shared;
        }

        let DirectoryData {
            entries,
            fingerprint,
            _hash,
        } = self.data;

        let entries = entries
            .into_iter()
            .map(|(k, v)| (k, v.map_dir(|d| d.shared(interner))))
            .collect();

        let new_data = DirectoryData {
            entries,
            fingerprint,
            _hash,
        };

        interner.intern(new_data)
    }

    pub fn into_entries<C>(self) -> C
    where
        C: FromIterator<(FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)>,
    {
        self.data
            .entries
            .into_iter()
            .map(|(k, v)| (k, v.map_dir(|v| v.into_builder())))
            .collect()
    }

    pub fn entries(
        &self,
    ) -> impl IntoIterator<Item = (&FileNameBuf, &DirectoryEntry<ImmutableDirectory<L, H>, L>)> + '_
    {
        &self.data.entries
    }

    pub fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a ImmutableDirectory<L, H>, &'a L>> {
        self.data.entries.get(needle).as_ref().map(|v| v.as_ref())
    }

    pub fn fingerprint(&self) -> &<H as HasDirectoryDigest>::Digest {
        self.data.fingerprint()
    }

    pub fn into_builder(self) -> DirectoryBuilder<L, H> {
        DirectoryBuilder::Immutable(ImmutableDirectory::Exclusive(self))
    }
}

impl_fingerprinted_directory!(ExclusiveDirectory);
