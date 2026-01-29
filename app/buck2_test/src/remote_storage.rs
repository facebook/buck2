/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashSet;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_error::Ok;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::re::manager::UnconfiguredRemoteExecutionClient;
use buck2_test_api::data::RemoteStorageConfig;
use dupe::Dupe;
use remote_execution::TDigest;

type CacheKey = TDigest;

pub struct ReClientWithCache {
    client: UnconfiguredRemoteExecutionClient,
    cache: Mutex<HashSet<Arc<CacheKey>>>,
}

impl ReClientWithCache {
    pub fn new(client: UnconfiguredRemoteExecutionClient) -> Self {
        Self {
            client,
            cache: Mutex::new(HashSet::new()),
        }
    }

    pub async fn apply_config(
        &self,
        artifact: &ArtifactValue,
        config: &RemoteStorageConfig,
    ) -> buck2_error::Result<()> {
        match &config.ttl_config {
            Some(ttl_config) => {
                // Note that deps represent artifacts that symlinks depend on. Currently, test artifact trees
                // that contain symlinks cannot be converted into remote objects. Therefore, we do not extend
                // the TTL of symlinks. Additionally, it is rare for test outputs to include symlinks, but if they do,
                // we are materializing them on disk.
                let digests = collect_digests(artifact.entry());

                // Filter out digests that are already in cache
                let digests_to_extend = {
                    let mut cache = self.cache.lock().unwrap();
                    let mut uncached_digests = Vec::new();
                    for digest in digests {
                        let digest = Arc::new(digest);
                        if !cache.contains(&digest) {
                            cache.insert(digest.dupe());
                            uncached_digests.push(Arc::unwrap_or_clone(digest));
                        }
                    }
                    uncached_digests
                };

                // Only extend TTL for digests not in cache
                if digests_to_extend.is_empty() {
                    return Ok(());
                }

                Ok(self
                    .client
                    .clone()
                    .with_use_case(ttl_config.use_case.dupe())
                    .extend_digest_ttl(digests_to_extend, ttl_config.ttl)
                    .await?)
            }
            _ => Ok(()),
        }
    }
}

fn collect_digests(directory_entry: &ActionDirectoryEntry<ActionSharedDirectory>) -> Vec<TDigest> {
    match directory_entry {
        ActionDirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => vec![f.digest.to_re()],
        ActionDirectoryEntry::Dir(dir) => {
            let mut digests: Vec<_> = dir
                .entries()
                .into_iter()
                .flat_map(|(_, entry)| collect_digests(entry))
                .collect();
            digests.push(dir.fingerprint().to_re());
            digests
        }
        _ => vec![], // Symlink or ExternalSymlink
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::file_ops::metadata::FileMetadata;
    use buck2_common::file_ops::metadata::TrackedFileDigest;
    use buck2_core::fs::project_rel_path::ProjectRelativePath;
    use buck2_execute::digest_config::DigestConfig;
    use buck2_execute::directory::ActionDirectoryBuilder;
    use buck2_execute::directory::extract_artifact_value;
    use buck2_execute::directory::insert_file;

    use super::*;

    #[test]
    fn test_collect_digests_dir() {
        let digest_config = DigestConfig::testing_default();
        let mut expected: Vec<TDigest> = vec![];
        let mut builder = ActionDirectoryBuilder::empty();

        // construct a directory with 3 files, f1, f2, and f3
        for file in &["d1/f1", "d1/f2", "d1/f3"] {
            let empty_file = FileMetadata {
                digest: TrackedFileDigest::from_content(
                    file.as_bytes(),
                    digest_config.cas_digest_config(),
                ),
                is_executable: false,
            };
            expected.push(empty_file.digest.to_re());
            let _unused = insert_file(
                &mut builder,
                ProjectRelativePath::new(file).unwrap().to_buf(),
                empty_file,
            );
        }

        let value = extract_artifact_value(
            &builder,
            ProjectRelativePath::new("d1").unwrap(),
            digest_config,
        )
        .expect("Failed to build dir")
        .unwrap();

        let digests = match value.entry() {
            ActionDirectoryEntry::Dir(dir) => {
                expected.push(dir.fingerprint().to_re());
                collect_digests(&value.entry())
            }
            _ => vec![],
        };
        assert_eq!(expected, digests);
    }

    #[test]
    fn test_collect_digests_leaf() {
        let digest_config = DigestConfig::testing_default();
        let empty_file = digest_config.empty_file();
        let expected = vec![empty_file.digest.to_re()];

        let leaf_entry: ActionDirectoryEntry<ActionSharedDirectory> =
            ActionDirectoryEntry::Leaf(ActionDirectoryMember::File(empty_file));
        let digests = collect_digests(&leaf_entry);
        assert_eq!(expected, digests);
    }
}
