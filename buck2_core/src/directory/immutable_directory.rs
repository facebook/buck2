/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_dupe))]

use derivative::Derivative;
use derive_more::Display;
use either::Either;
use gazebo::prelude::*;

use super::{
    impl_fingerprinted_directory, DashMapDirectoryInterner, Directory, DirectoryBuilder,
    DirectoryEntries, DirectoryEntry, DirectoryHasher, ExclusiveDirectory, FingerprintedDirectory,
    FingerprintedDirectoryEntries, HasDirectoryDigest, SharedDirectory,
};
use crate::fs::paths::{FileName, FileNameBuf};

#[derive(Derivative, Display)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "L: ::std::clone::Clone"))]
pub enum ImmutableDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    Exclusive(ExclusiveDirectory<L, H>),
    Shared(SharedDirectory<L, H>),
}

impl<L, H> ImmutableDirectory<L, H>
where
    H: HasDirectoryDigest,
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
}

impl<L, H> ImmutableDirectory<L, H>
where
    L: Clone,
    H: HasDirectoryDigest,
{
    pub fn into_entries<C>(self) -> C
    where
        C: FromIterator<(FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)>,
    {
        match self {
            Self::Exclusive(dir) => dir.into_entries(),
            Self::Shared(dir) => dir.into_entries(),
        }
    }
}

impl<L, H> Directory<L, H> for ImmutableDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    fn entries(&self) -> DirectoryEntries<'_, L, H> {
        match self {
            Self::Exclusive(dir) => Directory::entries(dir),
            Self::Shared(dir) => Directory::entries(dir),
        }
    }

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn Directory<L, H>, &'a L>> {
        match self {
            Self::Exclusive(dir) => Directory::get(dir, needle),
            Self::Shared(dir) => Directory::get(dir, needle),
        }
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
    H: HasDirectoryDigest,
{
    fn fingerprinted_entries<'a>(&'a self) -> FingerprintedDirectoryEntries<'a, L, H> {
        match self {
            Self::Exclusive(dir) => FingerprintedDirectory::fingerprinted_entries(dir),
            Self::Shared(dir) => FingerprintedDirectory::fingerprinted_entries(dir),
        }
    }

    fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a dyn FingerprintedDirectory<L, H>, &'a L>> {
        match self {
            Self::Exclusive(dir) => FingerprintedDirectory::get(dir, needle),
            Self::Shared(dir) => FingerprintedDirectory::get(dir, needle),
        }
    }

    fn fingerprint(&self) -> &<H as HasDirectoryDigest>::Digest {
        match self {
            Self::Exclusive(dir) => FingerprintedDirectory::fingerprint(dir),
            Self::Shared(dir) => FingerprintedDirectory::fingerprint(dir),
        }
    }
}

impl<L, H> PartialEq for ImmutableDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    fn eq(&self, other: &Self) -> bool {
        self.fingerprint() == other.fingerprint()
    }
}

impl<L, H> Eq for ImmutableDirectory<L, H> where H: HasDirectoryDigest {}
