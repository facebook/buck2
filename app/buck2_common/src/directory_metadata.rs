/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derive_more::Display;
use dupe::Dupe;

use crate::file_ops::TrackedFileDigest;

/// Fingerprint used to identify `ActionSharedDirectory`. We give it an explicit
/// alias because `TrackedFileDigest` can look confusing.
pub type ActionDirectoryFingerprint = TrackedFileDigest;

#[derive(Clone, Dupe, Debug, Display)]
#[display("DirectoryMetadata(digest:{},size:{})", fingerprint, total_size)]
pub struct DirectoryMetadata {
    pub fingerprint: ActionDirectoryFingerprint,
    /// Size on disk, if the artifact is a directory.
    /// Storing separately from ArtifactMetadata to avoid calculating when
    /// checking matching artifacts.
    pub total_size: u64,
}
