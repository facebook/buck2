/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;

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
        let mut walk = unordered_entry_walk(self.as_ref());
        while let Some((_path, entry)) = walk.next() {
            match entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => {
                    bytes += f.digest.size();
                    count += 1;
                }
                _ => {}
            }
        }
        OutputCountAndBytes { count, bytes }
    }
}
