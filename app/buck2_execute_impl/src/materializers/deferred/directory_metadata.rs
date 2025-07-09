/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::output_size::OutputSize;
use derive_more::Display;
use dupe::Dupe;

/// Fingerprint used to identify `ActionSharedDirectory`. We give it an explicit
/// alias because `TrackedFileDigest` can look confusing.
pub(crate) type ActionDirectoryFingerprint = TrackedFileDigest;

#[derive(Clone, Dupe, Debug, Display)]
pub(crate) enum DirectoryMetadata {
    /// Compact format where we only store the information necessary to identify a directory
    /// artifact. Gentle towards memory usage.
    #[display("Compact(digest:{},size:{})", fingerprint, total_size)]
    Compact {
        fingerprint: ActionDirectoryFingerprint,
        /// Size on disk
        total_size: u64,
    },
    Full(ActionSharedDirectory),
}

impl DirectoryMetadata {
    pub(crate) fn fingerprint(&self) -> &ActionDirectoryFingerprint {
        match self {
            DirectoryMetadata::Compact { fingerprint, .. } => fingerprint,
            DirectoryMetadata::Full(dir) => dir.fingerprint(),
        }
    }

    /// Size on disk, might need a calculation, so not always O(1).
    pub(crate) fn size(&self) -> u64 {
        match self {
            DirectoryMetadata::Compact { total_size, .. } => *total_size,
            DirectoryMetadata::Full(dir) => {
                DirectoryEntry::Dir(dir.dupe())
                    .calc_output_count_and_bytes()
                    .bytes
            }
        }
    }
}
