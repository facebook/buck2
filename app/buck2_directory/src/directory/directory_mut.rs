/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;

use buck2_fs::paths::file_name::FileName;

use crate::directory::directory::Directory;
use crate::directory::entry::DirectoryEntry;

/// A directory that isn't fingerprinted, and as such is mutable;
pub trait DirectoryMut<L, H>: Directory<L, H> {
    fn get_mut<'a>(
        &'a mut self,
        needle: &'_ FileName,
    ) -> Option<DirectoryEntry<&'a mut dyn DirectoryMut<L, H>, &'a mut L>>;
}

impl<L, H> fmt::Debug for &mut dyn DirectoryMut<L, H> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "DirectoryMut")
    }
}
