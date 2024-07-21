/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;

use crate::directory::entry::DirectoryEntry;
use crate::directory::fingerprinted_directory::FingerprintedDirectory;
use crate::fs::paths::file_name::FileName;

pub trait DirectoryDigest:
    Allocative + PartialEq + Eq + Hash + Clone + Dupe + Debug + Display
{
}

/// Indicates that this type of digest is suitable for use for interning.
///
/// Specifically, this is not implemented for `NoDigest`, as that returns the same `()` digest for
/// all directories.
pub trait InternableDirectoryDigest: DirectoryDigest {}

// TODO: Rename to DirectoryDigester
pub trait DirectoryHasher<L, H> {
    fn hash_entries<'a, D, I>(&self, entries: I) -> H
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<&'a D, &'a L>)>,
        D: FingerprintedDirectory<L, H> + 'a,
        L: 'a,
        Self: Sized;
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative, Display)]
#[display(fmt = "NoDigest")]
pub struct NoDigest(());

impl Dupe for NoDigest {}

impl DirectoryDigest for NoDigest {}

pub struct NoDigestDigester;

impl<L> DirectoryHasher<L, NoDigest> for NoDigestDigester {
    fn hash_entries<'a, D, I>(&self, _entries: I) -> NoDigest
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<&'a D, &'a L>)>,
        D: FingerprintedDirectory<L, NoDigest> + 'a,
        L: 'a,
        Self: Sized,
    {
        NoDigest(())
    }
}
