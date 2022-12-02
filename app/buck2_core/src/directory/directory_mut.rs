/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use super::Directory;
use super::DirectoryEntry;
use crate::fs::paths::file_name::FileName;

/// A directory that isn't fingerprinted, and as such is mutable;
pub trait DirectoryMut<L, H>: Directory<L, H> {
    fn get_mut<'a>(
        &'a mut self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a mut dyn DirectoryMut<L, H>, &'a mut L>>;
}

impl<'a, L, H> fmt::Debug for &'a mut dyn DirectoryMut<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DirectoryMut")
    }
}
