/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::directory_digest::InternableDirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use derivative::Derivative;
use derive_more::Display;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use crate::directory::directory::Directory;
use crate::directory::directory_data::DirectoryData;
use crate::directory::entry::DirectoryEntry;
use crate::directory::immutable_directory::ImmutableDirectory;
use crate::directory::immutable_or_exclusive::ImmutableOrExclusiveDirectoryRef;
use crate::directory::macros::impl_fingerprinted_directory;
use crate::directory::shared_directory::SharedDirectory;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
#[display("{}", self.data)]
pub struct ExclusiveDirectory<L, H>
where
    H: DirectoryDigest,
{
    pub(super) data: DirectoryData<ImmutableDirectory<L, H>, L, H>,
}

impl<L, H> ExclusiveDirectory<L, H>
where
    H: InternableDirectoryDigest,
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
}

impl<L, H> ExclusiveDirectory<L, H>
where
    H: DirectoryDigest,
{
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

    pub fn fingerprint(&self) -> &H {
        self.data.fingerprint()
    }

    pub fn into_builder(self) -> DirectoryBuilder<L, H> {
        DirectoryBuilder::Immutable(ImmutableDirectory::Exclusive(self))
    }
}

impl<L, H> Directory<L, H> for ExclusiveDirectory<L, H>
where
    H: DirectoryDigest,
{
    type DirectoryRef<'a> = ImmutableOrExclusiveDirectoryRef<'a, L, H>
    where
        Self: Sized + 'a,
        L: 'a;

    fn as_ref<'a>(&'a self) -> ImmutableOrExclusiveDirectoryRef<'a, L, H>
    where
        Self: Sized + 'a,
    {
        ImmutableOrExclusiveDirectoryRef::Exclusive(self)
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
    {
        self.clone().into_builder()
    }
}

impl_fingerprinted_directory!(ExclusiveDirectory);
