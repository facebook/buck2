/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::Directory;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::DirectoryIterator;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_execute::artifact::fs::ArtifactFs;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use serde::Serialize;
use serde::Serializer;

use crate::artifact_groups::ArtifactGroupValues;

fn stringify<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: Display,
    S: Serializer,
{
    serializer.collect_str(value)
}

pub(crate) fn metadata_content(
    fs: &ArtifactFs,
    inputs: &[&ArtifactGroupValues],
) -> anyhow::Result<(Vec<u8>, TrackedFileDigest)> {
    let mut builder = ActionDirectoryBuilder::empty();
    for &group in inputs {
        group.add_to_directory(&mut builder, fs)?;
    }

    #[derive(Serialize)]
    struct PathWithDigest<'a> {
        path: ForwardRelativePathBuf,
        #[serde(serialize_with = "stringify")]
        digest: &'a FileDigest,
    }

    #[derive(Serialize)]
    struct MetadataJson<'a> {
        version: i32,
        digests: Vec<PathWithDigest<'a>>,
    }

    let mut digests = Vec::new();
    let mut walk = builder.ordered_walk();
    while let Some((path, item)) = walk.next() {
        match item {
            DirectoryEntry::Leaf(ActionDirectoryMember::File(metadata)) => {
                digests.push(PathWithDigest {
                    path: path.get(),
                    digest: metadata.digest.data(),
                });
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

    let json = MetadataJson {
        digests,
        // Increment this version if format changes
        version: 1,
    };
    let json_string = serde_json::to_string(&json)?;
    let digest = TrackedFileDigest::new(FileDigest::from_bytes_sha1(json_string.as_bytes()));
    Ok((json_string.into(), digest))
}
