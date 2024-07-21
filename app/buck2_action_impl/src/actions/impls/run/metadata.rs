/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::artifact_groups::ArtifactGroupValues;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::directory::Directory;
use buck2_core::directory::directory_iterator::DirectoryIterator;
use buck2_core::directory::entry::DirectoryEntry;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::paths_with_digest::PathsWithDigestBlobData;
use buck2_execute::execute::paths_with_digest::PathsWithDigestBuilder;

pub(crate) fn metadata_content(
    fs: &ArtifactFs,
    inputs: &[&ArtifactGroupValues],
    digest_config: DigestConfig,
) -> anyhow::Result<(PathsWithDigestBlobData, TrackedFileDigest)> {
    let mut blob_builder = PathsWithDigestBuilder::default();

    let mut builder = ActionDirectoryBuilder::empty();
    for &group in inputs {
        group.add_to_directory(&mut builder, fs)?;
    }

    let mut walk = builder.ordered_walk();
    while let Some((path, item)) = walk.next() {
        match item {
            DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)) => {
                blob_builder.add(path.get(), metadata.digest.data());
            }
            // Omit symlinks and let user script detect and handle symlinks in inputs.
            // Metadata will contain artifacts which are symlinked, meaning the user
            // can resolve the symlink and get the digest of the symlinked artifact.
            DirectoryEntry::Leaf(ActionDirectoryMember::Symlink(_))
            | DirectoryEntry::Leaf(ActionDirectoryMember::ExternalSymlink(_)) => {}
            // Only interested in actual content, skip.
            DirectoryEntry::Dir(_) => {}
        }
    }

    let blob = blob_builder.build()?;

    let digest = TrackedFileDigest::from_content(&blob.0.0, digest_config.cas_digest_config());
    Ok((blob, digest))
}
