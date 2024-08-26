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
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;
use buck2_core::fs::paths::file_name::FileNameBuf;
use derivative::Derivative;
use derive_more::Display;
use dupe::Clone_;
use dupe::Dupe_;

use crate::directory::builder::DirectoryBuilder;
use crate::directory::dashmap_directory_interner::DashMapDirectoryInterner;
use crate::directory::directory::Directory;
use crate::directory::directory_data::DirectoryData;
use crate::directory::directory_ref::DirectoryRef;
use crate::directory::directory_ref::FingerprintedDirectoryRef;
use crate::directory::entry::DirectoryEntry;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;
use crate::directory::immutable_directory::ImmutableDirectory;
use crate::directory::macros::impl_fingerprinted_directory;

pub type SharedDirectoryData<L, H> = DirectoryData<SharedDirectory<L, H>, L, H>;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[display("{}", self.data)]
pub struct SharedDirectoryInner<L, H>
where
    H: DirectoryDigest,
{
    pub(super) data: SharedDirectoryData<L, H>,

    #[derivative(Debug = "ignore")]
    pub(super) interner: DashMapDirectoryInterner<L, H>,
}

impl<L, H> Drop for SharedDirectoryInner<L, H>
where
    H: DirectoryDigest,
{
    fn drop(&mut self) {
        self.interner.dropped(&self.data)
    }
}

#[derive(Derivative, Clone_, Dupe_, Display, Allocative)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[display("{}", self.inner)]
pub struct SharedDirectory<L, H>
where
    H: DirectoryDigest,
{
    pub(super) inner: Arc<SharedDirectoryInner<L, H>>,
}

impl<L, H> SharedDirectory<L, H>
where
    H: DirectoryDigest,
{
    pub fn as_immutable(self) -> ImmutableDirectory<L, H> {
        ImmutableDirectory::Shared(self)
    }

    pub fn entries(
        &self,
    ) -> impl IntoIterator<Item = (&FileNameBuf, &DirectoryEntry<SharedDirectory<L, H>, L>)> + '_
    {
        &self.inner.data.entries
    }

    pub fn get<'a>(
        &'a self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a SharedDirectory<L, H>, &'a L>> {
        self.inner
            .data
            .entries
            .get(needle)
            .as_ref()
            .map(|v| v.as_ref())
    }

    pub fn fingerprint(&self) -> &H {
        self.inner.data.fingerprint()
    }

    pub fn into_builder(self) -> DirectoryBuilder<L, H> {
        DirectoryBuilder::Immutable(self.as_immutable())
    }

    pub fn ptr_eq(&self, other: &SharedDirectory<L, H>) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }
}

impl<L, H> SharedDirectory<L, H>
where
    L: Clone,
    H: DirectoryDigest,
{
    pub fn into_entries<C>(self) -> C
    where
        C: FromIterator<(FileNameBuf, DirectoryEntry<DirectoryBuilder<L, H>, L>)>,
    {
        self.entries()
            .into_iter()
            .map(|(k, v)| (k.clone(), v.clone().map_dir(|v| v.into_builder())))
            .collect()
    }
}

pub struct SharedDirectoryEntries<'a, L, H>(
    sorted_vector_map::map::Iter<'a, FileNameBuf, DirectoryEntry<SharedDirectory<L, H>, L>>,
)
where
    H: DirectoryDigest;

impl<'a, L, H> Iterator for SharedDirectoryEntries<'a, L, H>
where
    H: DirectoryDigest,
{
    type Item = (
        &'a FileName,
        DirectoryEntry<&'a SharedDirectory<L, H>, &'a L>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        let (name, entry) = self.0.next()?;
        Some((name, entry.as_ref()))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.0.size_hint()
    }
}

impl<'a, L, H> DirectoryRef<'a> for &'a SharedDirectory<L, H>
where
    H: DirectoryDigest,
{
    type Leaf = L;
    type DirectoryDigest = H;
    type Entries = SharedDirectoryEntries<'a, L, H>;

    fn get(self, name: &FileName) -> Option<DirectoryEntry<Self, &'a Self::Leaf>> {
        self.get(name)
    }

    fn entries(self) -> Self::Entries {
        SharedDirectoryEntries(self.inner.data.entries.iter())
    }

    fn as_dyn(self) -> &'a dyn Directory<Self::Leaf, Self::DirectoryDigest> {
        self
    }
}

impl<'a, L, H> FingerprintedDirectoryRef<'a> for &'a SharedDirectory<L, H>
where
    H: DirectoryDigest,
{
    fn as_fingerprinted_dyn(
        self,
    ) -> &'a dyn FingerprintedDirectory<Self::Leaf, Self::DirectoryDigest> {
        self
    }
}

impl<L, H> Directory<L, H> for SharedDirectory<L, H>
where
    H: DirectoryDigest,
{
    type DirectoryRef<'a> = &'a SharedDirectory<L, H> where Self: Sized + 'a;

    fn as_ref<'a>(&'a self) -> Self::DirectoryRef<'a>
    where
        Self: Sized + 'a,
    {
        self
    }

    fn to_builder(&self) -> DirectoryBuilder<L, H>
    where
        L: Clone,
    {
        self.clone().into_builder()
    }
}

impl_fingerprinted_directory!(SharedDirectory);
