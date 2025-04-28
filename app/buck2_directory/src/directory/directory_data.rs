/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileNameBuf;
use derivative::Derivative;
use derive_more::Display;
use sorted_vector_map::SortedVectorMap;

use crate::directory::directory_hasher::DirectoryHasher;
use crate::directory::entry::DirectoryEntry;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;

#[derive(Derivative, Display, Allocative)]
#[derivative(Debug(bound = "D: ::std::fmt::Debug, L: ::std::fmt::Debug"))]
#[derivative(Clone(bound = "D: ::std::clone::Clone, L: ::std::clone::Clone"))]
#[display("Directory({})", self.fingerprint)]
pub struct DirectoryData<D, L, H>
where
    H: DirectoryDigest,
{
    /// SortedVectorMap is a more compact immutatable representation for directories.
    /// Experimentally, it takes about 30% less space, while resulting in no runtime regression.
    pub entries: SortedVectorMap<FileNameBuf, DirectoryEntry<D, L>>,

    pub(super) fingerprint: H,

    #[derivative(Debug = "ignore")]
    pub(super) _hash: PhantomData<H>,
}

impl<D, L, H> DirectoryData<D, L, H>
where
    H: DirectoryDigest,
{
    pub fn fingerprint(&self) -> &H {
        &self.fingerprint
    }
}

impl<D, L, H> DirectoryData<D, L, H>
where
    H: DirectoryDigest,
    D: FingerprintedDirectory<L, H>,
{
    pub fn new(
        entries: SortedVectorMap<FileNameBuf, DirectoryEntry<D, L>>,
        hasher: &impl DirectoryHasher<L, H>,
    ) -> Self {
        let fingerprint = hasher.hash_entries(
            entries
                .iter()
                .map(|(k, e)| (k.as_ref(), e.as_ref().map_dir(|d| d.as_fingerprinted_ref()))),
        );
        Self {
            entries,
            fingerprint,
            _hash: PhantomData,
        }
    }
}
