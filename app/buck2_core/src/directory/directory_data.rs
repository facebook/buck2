/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", allow(gazebo_lint_use_dupe))]

use std::marker::PhantomData;

use allocative::Allocative;
use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;
use sorted_vector_map::SortedVectorMap;

use super::DirectoryEntry;
use super::DirectoryHasher;
use super::FingerprintedDirectory;
use super::HasDirectoryDigest;
use crate::fs::paths::file_name::FileNameBuf;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "D: ::std::fmt::Debug, L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "D: ::std::clone::Clone, L: ::std::clone::Clone"))]
#[display(fmt = "Directory({})", "self.fingerprint")]
pub struct DirectoryData<D, L, H>
where
    H: HasDirectoryDigest,
{
    /// SortedVectorMap is a more compact immutatable representation for directories.
    /// Experimentally, it takes about 30% less space, while resulting in no runtime regression.
    pub entries: SortedVectorMap<FileNameBuf, DirectoryEntry<D, L>>,

    pub(super) fingerprint: <H as HasDirectoryDigest>::Digest,

    #[derivative(Debug = "ignore")]
    pub(super) _hash: PhantomData<H>,
}

impl<D, L, H> DirectoryData<D, L, H>
where
    H: HasDirectoryDigest,
{
    pub fn fingerprint(&self) -> &<H as HasDirectoryDigest>::Digest {
        &self.fingerprint
    }
}

impl<D, L, H> DirectoryData<D, L, H>
where
    H: DirectoryHasher<L>,
    D: FingerprintedDirectory<L, H>,
{
    pub fn new(entries: SortedVectorMap<FileNameBuf, DirectoryEntry<D, L>>) -> Self {
        let fingerprint = H::hash_entries(entries.iter().map(|(k, e)| (k.as_ref(), e.as_ref())));
        Self {
            entries,
            fingerprint,
            _hash: PhantomData,
        }
    }
}
