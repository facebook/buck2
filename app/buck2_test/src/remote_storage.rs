/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Duration;

use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use buck2_test_api::data::CasDigest;
use buck2_test_api::data::RemoteObject;
use buck2_test_api::data::RemoteStorageConfig;
use dupe::Dupe;

use crate::translations::convert_test_to_re_digest;

// TODO(arr): consider taking `ArtifactValue` here to avoid CasDigest -> TDigest
// conversions
pub async fn apply_config(
    client: ManagedRemoteExecutionClient,
    object: &RemoteObject,
    config: &RemoteStorageConfig,
) -> anyhow::Result<()> {
    match &config.ttl_config {
        Some(ttl_config) => {
            extend_object_ttl(client, object, ttl_config.ttl, ttl_config.use_case.dupe()).await
        }
        _ => Ok(()),
    }
}

async fn extend_object_ttl(
    client: ManagedRemoteExecutionClient,
    object: &RemoteObject,
    ttl: Duration,
    use_case: RemoteExecutorUseCase,
) -> anyhow::Result<()> {
    let digests = collect_digests(object)
        .map(convert_test_to_re_digest)
        .collect();
    client.extend_digest_ttl(digests, ttl, use_case).await
}

fn collect_digests(obj: &RemoteObject) -> Box<dyn Iterator<Item = CasDigest> + '_> {
    match obj {
        RemoteObject::File(file) => Box::new(std::iter::once(file.digest.clone())),
        RemoteObject::Dir(dir) => Box::new(
            std::iter::once(dir.digest.clone())
                .chain(dir.children.iter().map(collect_digests).flatten()),
        ),
    }
}

#[cfg(test)]
mod tests {
    use buck2_test_api::data::RemoteDir;
    use buck2_test_api::data::RemoteFile;

    use super::*;

    fn make_digest(hash: &str, size: i64) -> CasDigest {
        CasDigest {
            hash: hash.to_owned(),
            size_bytes: size,
        }
    }

    #[test]
    fn test_collect_digests() {
        let object = RemoteObject::Dir(RemoteDir {
            digest: make_digest("A", 1),
            name: "root".to_owned(),
            children: vec![
                RemoteObject::File(RemoteFile {
                    digest: make_digest("B", 2),
                    name: "file1".to_owned(),
                }),
                RemoteObject::Dir(RemoteDir {
                    digest: make_digest("C", 3),
                    name: "subdir".to_owned(),
                    children: vec![RemoteObject::File(RemoteFile {
                        digest: make_digest("D", 4),
                        name: "file2".to_owned(),
                    })],
                }),
                RemoteObject::File(RemoteFile {
                    digest: make_digest("E", 5),
                    name: "file3".to_owned(),
                }),
            ],
        });
        let digests: Vec<_> = collect_digests(&object).collect();
        assert_eq!(
            vec![
                make_digest("A", 1),
                make_digest("B", 2),
                make_digest("C", 3),
                make_digest("D", 4),
                make_digest("E", 5),
            ],
            digests
        )
    }
}
