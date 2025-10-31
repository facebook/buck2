/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileNameBuf;
use derivative::Derivative;
use derive_more::Display;
use either::Either;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use crate::directory::directory::Directory;
use crate::directory::entry::DirectoryEntry;
use crate::directory::exclusive_directory::ExclusiveDirectory;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;
use crate::directory::immutable_or_exclusive::ImmutableOrExclusiveDirectoryRef;
use crate::directory::shared_directory::SharedDirectory;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
pub enum ImmutableDirectory<L, H>
where
    H: DirectoryDigest,
{
    Exclusive(ExclusiveDirectory<L, H>),
    Shared(SharedDirectory<L, H>),
}

impl<L, H> ImmutableDirectory<L, H>
where
    H: DirectoryDigest,
{
    pub fn shared(self, interner: &DashMapDirectoryInterner<L, H>) -> SharedDirectory<L, H> {
        match self {
            Self::Exclusive(dir) => dir.shared(interner),
            Self::Shared(dir) => dir,
        }
    }

    pub fn into_builder(self) -> DirectoryBuilder<L, H> {
        match self {
            Self::Exclusive(d) => d.into_builder(),
            Self::Shared(s) => s.into_builder(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Exclusive(dir) => dir.data.entries.len(),
            Self::Shared(dir) => dir.inner.data.entries.len(),
        }
    }
}

impl<L, H> ImmutableDirectory<L, H>
where
    L: Clone,
    H: DirectoryDigest,
{
    pub fn collect_entries<C>(self) -> C
    where
        C: FromIterator<(FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)>,
    {
        match self {
            Self::Exclusive(dir) => dir.collect_entries(),
            Self::Shared(dir) => dir.collect_entries(),
        }
    }

    pub fn into_entries(
        self,
    ) -> impl Iterator<Item = (FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)> {
        match self {
            Self::Exclusive(dir) => Either::Left(dir.into_entries()),
            Self::Shared(dir) => Either::Right(dir.into_entries()),
        }
    }
}

impl<L, H> Directory<L, H> for ImmutableDirectory<L, H>
where
    H: DirectoryDigest,
{
    type DirectoryRef<'a>
        = ImmutableOrExclusiveDirectoryRef<'a, L, H>
    where
        Self: Sized + 'a,
        L: 'a;

    fn as_ref<'a>(&'a self) -> Self::DirectoryRef<'a>
    where
        Self: Sized + 'a,
    {
        ImmutableOrExclusiveDirectoryRef::from_immutable(self)
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
    {
        self.clone().into_builder()
    }
}

impl<L, H> FingerprintedDirectory<L, H> for ImmutableDirectory<L, H>
where
    H: DirectoryDigest,
{
    type FingerprintedDirectoryRef<'a>
        = ImmutableOrExclusiveDirectoryRef<'a, L, H>
    where
        Self: Sized + 'a,
        L: 'a;

    fn as_fingerprinted_ref<'a>(&'a self) -> Self::FingerprintedDirectoryRef<'a>
    where
        Self: Sized + 'a,
    {
        self.as_ref()
    }

    fn fingerprint(&self) -> &H {
        match self {
            Self::Exclusive(dir) => FingerprintedDirectory::fingerprint(dir),
            Self::Shared(dir) => FingerprintedDirectory::fingerprint(dir),
        }
    }

    fn size(&self) -> u64 {
        match self {
            Self::Exclusive(dir) => FingerprintedDirectory::size(dir),
            Self::Shared(dir) => FingerprintedDirectory::size(dir),
        }
    }
}

impl<L, H> PartialEq for ImmutableDirectory<L, H>
where
    H: DirectoryDigest,
{
    fn eq(&self, other: &Self) -> bool {
        self.fingerprint() == other.fingerprint()
    }
}

impl<L, H> Eq for ImmutableDirectory<L, H> where H: DirectoryDigest {}
