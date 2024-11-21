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
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::paths_with_digest::PathsWithDigestBlobData;
use buck2_execute::execute::paths_with_digest::PathsWithDigestBuilder;

pub(crate) fn metadata_content(
    fs: &ArtifactFs,
    inputs: &[&ArtifactGroupValues],
    digest_config: DigestConfig,
) -> buck2_error::Result<(PathsWithDigestBlobData, TrackedFileDigest)> {
    let mut blob_builder = PathsWithDigestBuilder::default();

    let mut builder = ActionDirectoryBuilder::empty();
    for &group in inputs {
        group.add_to_directory(&mut builder, fs)?;
    }

    let mut walk = builder.ordered_walk_leaves();
    while let Some((path, item)) = walk.next() {
        match item {
            ActionDirectoryMember::File(metadata) => {
                blob_builder.add(path.get(), metadata.digest.data());
            }
            // Omit symlinks and let user script detect and handle symlinks in inputs.
            // Metadata will contain artifacts which are symlinked, meaning the user
            // can resolve the symlink and get the digest of the symlinked artifact.
            ActionDirectoryMember::Symlink(_) | ActionDirectoryMember::ExternalSymlink(_) => {}
        }
    }

    let blob = blob_builder.build()?;

    let digest = TrackedFileDigest::from_content(&blob.0.0, digest_config.cas_digest_config());
    Ok((blob, digest))
}
