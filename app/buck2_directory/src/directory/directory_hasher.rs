/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::hash::Hash;

use allocative::Allocative;
use buck2_core::directory_digest::DirectoryDigest;
use buck2_core::fs::paths::file_name::FileName;
use derive_more::Display;
use dupe::Dupe;

use crate::directory::directory_ref::FingerprintedDirectoryRef;
use crate::directory::entry::DirectoryEntry;

pub trait DirectoryDigester<L, H> {
    fn hash_entries<'a, D, I>(&self, entries: I) -> H
    where
        I: IntoIterator<Item = (&'a FileName, DirectoryEntry<D, &'a L>)>,
        D: FingerprintedDirectoryRef<'a, Leaf = L, DirectoryDigest = H> + 'a,
        L: 'a,
        Self: Sized;
}

#[allow(unused)]
#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative, Display)]
pub struct NoDigest(!);

impl Dupe for NoDigest {}

impl DirectoryDigest for NoDigest {}
