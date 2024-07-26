/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_core::directory_digest::DirectoryDigest;

use crate::directory::directory::Directory;
use crate::directory::directory_ref::FingerprintedDirectoryRef;

pub trait FingerprintedDirectory<L, H>: Directory<L, H> {
    type FingerprintedDirectoryRef<'a>: FingerprintedDirectoryRef<'a, Leaf = L, DirectoryDigest = H>
    where
        Self: Sized + 'a,
        L: 'a;

    fn as_fingerprinted_ref<'a>(&'a self) -> Self::FingerprintedDirectoryRef<'a>
    where
        Self: Sized + 'a;

    fn fingerprint(&self) -> &H
    where
        H: DirectoryDigest;
}

impl<'a, L, H> fmt::Debug for &'a dyn FingerprintedDirectory<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FingerprintedDirectory")
    }
}
