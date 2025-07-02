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
use derive_more::Display;
use dupe::Dupe;

/// Fingerprint used to identify `ActionSharedDirectory`. We give it an explicit
/// alias because `TrackedFileDigest` can look confusing.
pub(crate) type ActionDirectoryFingerprint = TrackedFileDigest;

#[derive(Clone, Dupe, Debug, Display)]
#[display("DirectoryMetadata(digest:{},size:{})", fingerprint, total_size)]
pub(crate) struct DirectoryMetadata {
    pub(crate) fingerprint: ActionDirectoryFingerprint,
    /// Size on disk
    pub(crate) total_size: u64,
}
