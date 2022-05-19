/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_dupe))]

use std::{
    any::Any,
    sync::{Arc, Weak},
};

use dashmap::{mapref::entry::Entry, DashMap};
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;

use super::{
    impl_fingerprinted_directory, DashMapDirectoryInterner, Directory, DirectoryBuilder,
    DirectoryData, DirectoryEntries, DirectoryEntry, DirectoryHasher, FingerprintedDirectory,
    FingerprintedDirectoryEntries, HasDirectoryDigest, ImmutableDirectory,
};
use crate::fs::paths::{FileName, FileNameBuf};

pub type SharedDirectoryData<L, H> = DirectoryData<SharedDirectory<L, H>, L, H>;

#[derive(Derivative, Display)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[display(fmt = "{}", "self.data")]
pub struct SharedDirectoryInner<L, H>
where
    H: HasDirectoryDigest,
{
    pub(super) data: SharedDirectoryData<L, H>,

    #[derivative(Debug = "ignore")]
    pub(super) interner: DashMapDirectoryInterner<L, H>,
}

impl<L, H> Drop for SharedDirectoryInner<L, H>
where
    H: HasDirectoryDigest,
{
    fn drop(&mut self) {
        self.interner.dropped(&self.data)
    }
}

#[derive(Derivative, Clone_, Dupe_, Display)]
#[derivative(Debug(bound = "L: ::std::fmt::Debug"))]
#[display(fmt = "{}", "self.inner")]
pub struct SharedDirectory<L, H>
where
    H: HasDirectoryDigest,
{
    pub(super) inner: Arc<SharedDirectoryInner<L, H>>,
}

impl<L, H> SharedDirectory<L, H>
where
    H: HasDirectoryDigest,
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

    pub fn fingerprint(&self) -> &<H as HasDirectoryDigest>::Digest {
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
    H: HasDirectoryDigest,
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

impl_fingerprinted_directory!(SharedDirectory);
