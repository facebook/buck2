/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;

use crate::artifact_value::ArtifactValue;
use crate::directory::ActionDirectory;
use crate::directory::ActionDirectoryMember;

pub struct OutputCountAndBytes {
    pub count: u64,
    pub bytes: u64,
}
pub trait OutputSize {
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes;
}

impl OutputSize for ArtifactValue {
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes {
        self.entry().calc_output_count_and_bytes()
    }
}

impl<D> OutputSize for DirectoryEntry<D, ActionDirectoryMember>
where
    D: ActionDirectory,
{
    fn calc_output_count_and_bytes(&self) -> OutputCountAndBytes {
        let mut bytes = 0;
        let mut count = 0;
        let mut walk = unordered_entry_walk(self.as_ref().map_dir(|d| Directory::as_ref(d)));
        while let Some((_path, entry)) = walk.next() {
            if let DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) = entry {
                bytes += f.digest.size();
                count += 1;
            }
        }
        OutputCountAndBytes { count, bytes }
    }
}
