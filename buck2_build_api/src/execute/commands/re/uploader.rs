/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
    sync::Arc,
    time::{Duration, SystemTime},
};

use anyhow::Context;
use buck2_common::file_ops::FileDigest;
use buck2_core::{
    directory::{DirectoryEntry, DirectoryIterator},
    env_helper::EnvHelper,
};
use gazebo::prelude::*;
use prost::Message;
use remote_execution::{
    GetDigestsTtlRequest, InlinedBlobWithDigest, NamedDigest, REClient, REClientError,
    RemoteExecutionMetadata, TCode, TDigest, UploadRequest,
};

use crate::{
    actions::{
        digest::FileDigestReExt,
        directory::{
            ActionDirectoryMember, ActionFingerprintedDirectory, ActionImmutableDirectory,
            ReDirectorySerializer,
        },
    },
    execute::materializer::{ArtifactNotMaterializedReason, CasDownloadInfo, Materializer},
};

/// Contains small blobs referenced from action messages (does not include any file contents blobs).
pub struct ActionBlobs(HashMap<FileDigest, Vec<u8>>);

impl ActionBlobs {
    pub fn new() -> Self {
        // We add empty files to the input that don't exist in disk; so add
        // the empty digest to blobs, as going to disk would fail.
        let mut blobs = HashMap::new();
        blobs.insert(FileDigest::empty(), Vec::new());
        Self(blobs)
    }

    pub(crate) fn add_blob(&mut self, digest: FileDigest, data: Vec<u8>) {
        self.0.insert(digest, data);
    }

    pub(crate) fn add_protobuf_message(&mut self, m: &impl Message) -> FileDigest {
        let mut blob = Vec::new();
        // Unwrap is safe because it only fails in OOM conditions, which we pretend don't happen
        m.encode(&mut blob).unwrap();
        let digest = FileDigest::from_bytes(&blob);
        self.0.insert(digest.dupe(), blob);
        digest
    }

    fn keys(&self) -> impl Iterator<Item = &FileDigest> {
        self.0.keys()
    }

    fn get(&self, digest: &FileDigest) -> Option<&Vec<u8>> {
        self.0.get(digest)
    }
}

pub struct Uploader {}

impl Uploader {
    pub(crate) async fn upload(
        client: &REClient,
        materializer: Arc<dyn Materializer>,
        input_dir: &ActionImmutableDirectory,
        blobs: &ActionBlobs,
        metadata: RemoteExecutionMetadata,
    ) -> anyhow::Result<()> {
        let now = SystemTime::now();
        let deadline = now + Duration::from_secs(600);

        // See if anything needs uploading
        let mut input_digests = blobs.keys().collect::<HashSet<_>>();
        let digest_ttls = {
            // Collect the digests we need to upload
            for entry in input_dir.fingerprinted_unordered_walk().without_paths() {
                let digest = match entry {
                    DirectoryEntry::Dir(d) => d.fingerprint(),
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => &f.digest,
                    DirectoryEntry::Leaf(..) => continue,
                };

                if digest.expires() <= deadline {
                    input_digests.insert(digest);
                }
            }

            let root_dir_digest = input_dir.fingerprint();
            if root_dir_digest.expires() <= deadline {
                input_digests.insert(root_dir_digest);
            }

            // Find out which ones are missing
            let request = GetDigestsTtlRequest {
                digests: input_digests.iter().map(|d| d.to_re()).collect(),
                ..Default::default()
            };
            client
                .get_digests_ttl(metadata.clone(), request)
                .await?
                .digests_with_ttl
        };

        let mut upload_blobs = Vec::new();
        let mut missing_digests = HashSet::new();
        add_injected_missing_digests(&input_digests, &mut missing_digests)?;

        let mut input_digests = input_digests.into_iter().collect::<Vec<_>>();
        input_digests.sort();

        let mut digest_ttls = digest_ttls.into_map(|d| (FileDigest::from_re(&d.digest), d.ttl));
        digest_ttls.sort();

        if input_digests.len() != digest_ttls.len() {
            return Err(anyhow::anyhow!(
                "Invalid response from get_digests_ttl: expected {}, got {} digests",
                input_digests.len(),
                digest_ttls.len()
            ));
        }

        // Find the blobs that need to be uploaded

        for (digest, (matching_digest, digest_ttl)) in
            input_digests.into_iter().zip(digest_ttls.into_iter())
        {
            if *digest != matching_digest {
                return Err(anyhow::anyhow!("Invalid response from get_digests_ttl"));
            }

            if digest_ttl <= 0 {
                match blobs.get(digest) {
                    Some(blob) => {
                        upload_blobs.push(InlinedBlobWithDigest {
                            blob: blob.clone(),
                            digest: digest.to_re(),
                            ..Default::default()
                        });
                    }
                    None => {
                        missing_digests.insert(digest);
                    }
                }
            } else {
                let ttl = Duration::from_secs(digest_ttl as u64);
                digest.update_expires(now + ttl);
            }
        }

        if upload_blobs.is_empty() && missing_digests.is_empty() {
            return Ok(());
        }

        // Find the file paths and directory blobs that need to be uploaded
        let mut upload_files = Vec::new();

        // Track what files should be materialized before we upload.
        let mut paths_to_materialize = Vec::new();

        if !missing_digests.is_empty() {
            let mut upload_file_paths = Vec::new();
            let mut upload_file_digests = Vec::new();

            {
                let mut walk = input_dir.fingerprinted_unordered_walk();
                while let Some((path, entry)) = walk.next() {
                    let digest = match entry {
                        DirectoryEntry::Dir(d) => d.fingerprint(),
                        DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => &f.digest,
                        DirectoryEntry::Leaf(..) => continue,
                    };

                    if !missing_digests.remove(digest) {
                        continue;
                    }

                    match entry {
                        DirectoryEntry::Dir(d) => {
                            upload_blobs.push(directory_to_blob(d));
                        }
                        DirectoryEntry::Leaf(ActionDirectoryMember::File(..)) => {
                            upload_file_paths.push(path.get().into());
                            upload_file_digests.push(digest.to_re());
                        }
                        DirectoryEntry::Leaf(..) => unreachable!(), // TODO: Better representation of this.
                    };
                }
            }

            if missing_digests.remove(input_dir.fingerprint()) {
                upload_blobs.push(directory_to_blob(input_dir));
            }

            assert!(
                missing_digests.is_empty(),
                "Expected a path to be found for every digest, traversal code is inconsistent. Left with {:?}.",
                missing_digests
            );

            // Get the real path of the files we are going to upload.
            // This needs to be done because we could have A copied to B, and
            // we are asked to upload B. But since we defer local copies until
            // it's actually needed for a local run, B might not have been
            // copied yet (or ever), so we should upload A directly instead.
            let upload_file_paths = materializer
                .get_materialized_file_paths(upload_file_paths)
                .await?;

            for (name, digest) in upload_file_paths
                .into_iter()
                .zip(upload_file_digests.into_iter())
            {
                match name {
                    Ok(name) => {
                        upload_files.push(NamedDigest {
                            name: name.to_string(),
                            digest,
                            ..Default::default()
                        });
                    }
                    Err(
                        ref err @ ArtifactNotMaterializedReason::RequiresCasDownload {
                            ref entry,
                            ref info,
                            ..
                        },
                    ) => {
                        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(ref file)) =
                            entry.as_ref()
                        {
                            // NOTE: find_missing has negative caching, so when we query to know if an
                            // artifact was uploaded, if it was the result of an action we just ran, it
                            // won't be here. On the flip side, if a digest has been in the CAS for
                            // a very long time, it might have expired.
                            if file.digest.to_re() == digest {
                                if should_error_for_missing_digest(info) {
                                    return Err(anyhow::anyhow!(
                                        "Your build requires an artifact that has expired in the RE CAS \
                                        and Buck does not have it. This likely happened because your Buck daemon \
                                        has been online for a long time. This error is currently unrecoverable. \
                                        To proceed, you should restart Buck using `buck2 kill`. \
                                        Debug information: {:#}",
                                        err
                                    ));
                                }

                                tracing::debug!(
                                    "{} is missing in the CAS but expected to exist as per: {:#}",
                                    digest,
                                    err
                                );
                                continue;
                            }
                        }

                        return Err(error_for_missing_file(&digest, err));
                    }
                    Err(ArtifactNotMaterializedReason::RequiresHttpDownload { path }) => {
                        upload_files.push(NamedDigest {
                            name: path.to_string(),
                            digest,
                            ..Default::default()
                        });
                        paths_to_materialize.push(path);
                    }
                    Err(
                        ref err @ ArtifactNotMaterializedReason::DeferredMaterializerCorruption {
                            ..
                        },
                    ) => {
                        return Err(error_for_missing_file(&digest, err));
                    }
                };
            }
        }

        if !paths_to_materialize.is_empty() {
            materializer
                .ensure_materialized(paths_to_materialize)
                .await
                .context("Error materializing paths for upload")?;
        }

        // Upload
        let upload_res = if !upload_files.is_empty() || !upload_blobs.is_empty() {
            client
                .upload(
                    metadata,
                    UploadRequest {
                        files_with_digest: Some(upload_files),
                        inlined_blobs_with_digest: Some(upload_blobs),
                        // all find missing checks are done previously
                        // and we can skip them and upload all digests
                        upload_only_missing: false,
                        ..Default::default()
                    },
                )
                .await
                .map(|_| ())
        } else {
            Ok(())
        };

        if let Err(e) = upload_res.as_ref() {
            if let Some(re_client_error) = e.downcast_ref::<REClientError>() {
                if re_client_error.code == TCode::INVALID_ARGUMENT {
                    return Err(anyhow::anyhow!(
                        "RE Upload failed. It looks like you might have modified files while the build \
                        was in progress. Retry your build to proceed. Debug information: {:#}",
                        e
                    ));
                }
            }
        }

        upload_res.context("RE: upload")?;

        Ok(())
    }
}

fn should_error_for_missing_digest(info: &CasDownloadInfo) -> bool {
    // RE sometimes reports things that exist as missing. We don't fully understand why at this
    // time and this is being investigated, but we know that RE normally ensures that anything it
    // returns to us will last for another 6 hours at least. So, we silence all errors when the
    // download info is less than 5 hours, because we know those are most likely bogus. This
    // basically means that after 5 hours we might tell the user to restart even though they don't
    // need to. However, the alternative is confused users who see errors after a couple days (we
    // had 3 reports of this in a week), so for now we do accept some false positives (when RE
    // tells us a digest doesn't exist even though it does) in order to provide better UX when we
    // hit a true positive.
    info.action_age() >= Duration::from_secs(3600 * 5)
}

fn directory_to_blob<D>(d: &D) -> InlinedBlobWithDigest
where
    D: ActionFingerprintedDirectory + ?Sized,
{
    InlinedBlobWithDigest {
        digest: d.fingerprint().to_re(),
        blob: ReDirectorySerializer::serialize_entries(d.fingerprinted_entries()),
        ..Default::default()
    }
}

fn error_for_missing_file(
    digest: &TDigest,
    cause: &ArtifactNotMaterializedReason,
) -> anyhow::Error {
    anyhow::anyhow!(
        "Action execution requires artifact `{}` but the materializer did not return a matching \
        file for this path. This error is unrecoverable and you should restart Buck using \
        `buck2 kill`. We would appreciate a bug report. Debug information: {:#}",
        digest,
        cause,
    )
}

/// This is used for tests. We allow an environment variable to be set to report that some digests
/// are _always_ missing if they are required. This lets us test our upload paths more easily.
fn add_injected_missing_digests(
    input_digests: &HashSet<&FileDigest>,
    missing_digests: &mut HashSet<&FileDigest>,
) -> anyhow::Result<()> {
    fn convert_digests(val: &str) -> anyhow::Result<Vec<FileDigest>> {
        val.split(' ')
            .map(|digest| {
                let digest = TDigest::from_str(digest)
                    .with_context(|| format!("Invalid digest: `{}`", digest))?;
                let digest = FileDigest::from_re(&digest);
                anyhow::Ok(digest)
            })
            .collect()
    }

    static INJECTED_DIGESTS: EnvHelper<Vec<FileDigest>> =
        EnvHelper::with_converter("BUCK2_TEST_INJECTED_MISSING_DIGESTS", convert_digests);

    if let Some(digests) = INJECTED_DIGESTS.get()?.as_ref() {
        for d in digests {
            if input_digests.contains(&d) {
                missing_digests.insert(d);
            }
        }
    }

    Ok(())
}
