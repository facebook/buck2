/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::collections::HashMap;
use std::collections::HashSet;
use std::path::Path;
use std::str::FromStr;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_common::cas_digest::TrackedCasDigest;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::FileDigestKind;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_core::buck2_env;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::soft_error;
use buck2_data::ReUploadMetrics;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::directory_ref::FingerprintedDirectoryRef;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::fingerprinted_directory::FingerprintedDirectory;
use buck2_error::BuckErrorContext;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use chrono::Duration;
use chrono::Utc;
use dupe::Dupe;
use either::Either;
use futures::FutureExt;
use futures::TryStreamExt;
use futures::future::BoxFuture;
use futures::future::Shared;
use futures::stream::FuturesUnordered;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use remote_execution::GetDigestsTtlResponse;
use remote_execution::InlinedBlobWithDigest;
use remote_execution::NamedDigest;
use remote_execution::TDigest;
use remote_execution::UploadRequest;

use crate::digest::CasDigestFromReExt;
use crate::digest::CasDigestToReExt;
use crate::digest_config::DigestConfig;
use crate::directory::ActionDirectoryMember;
use crate::directory::ActionFingerprintedDirectoryRef;
use crate::directory::ActionImmutableDirectory;
use crate::directory::ReDirectorySerializer;
use crate::execute::blobs::ActionBlobs;
use crate::materialize::materializer::ArtifactNotMaterializedReason;
use crate::materialize::materializer::CasDownloadInfo;
use crate::materialize::materializer::Materializer;
use crate::re::action_identity::ReActionIdentity;
use crate::re::client::RemoteExecutionClient;
use crate::re::error::with_error_handler;
use crate::re::metadata::RemoteExecutionMetadataExt;

#[derive(Clone, Debug, Default)]
pub struct UploadStats {
    pub total: ReUploadMetrics,
    pub by_extension: HashMap<String, ReUploadMetrics>,
}

pub struct Uploader {}

impl Uploader {
    async fn find_missing<'a>(
        client: &RemoteExecutionClient,
        input_dir: &'a ActionImmutableDirectory,
        blobs: &'a ActionBlobs,
        use_case: &RemoteExecutorUseCase,
        identity: Option<&ReActionIdentity<'_>>,
        digest_config: DigestConfig,
        deduplicate_get_digests_ttl_calls: bool,
    ) -> buck2_error::Result<(
        Vec<InlinedBlobWithDigest>,
        HashSet<&'a TrackedCasDigest<FileDigestKind>>,
    )> {
        // RE mentions they usually take 5-10 minutes of leeway so we mirror this here.
        let now = Utc::now();
        let ttl_wanted = if buck2_core::is_open_source() {
            1
        } else {
            600i64
        };
        let ttl_deadline = now + Duration::seconds(ttl_wanted);

        // See if anything needs uploading
        let mut input_digests = blobs.keys().collect::<HashSet<_>>();
        {
            // Collect the digests we need to upload
            for entry in input_dir.unordered_walk().without_paths() {
                let digest = match entry {
                    DirectoryEntry::Dir(d) => d.as_fingerprinted_dyn().fingerprint(),
                    DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) => &f.digest,
                    DirectoryEntry::Leaf(..) => continue,
                };

                if digest.expires()? <= ttl_deadline {
                    input_digests.insert(digest);
                }
            }

            let root_dir_digest = input_dir.fingerprint();
            if root_dir_digest.expires()? <= ttl_deadline {
                input_digests.insert(root_dir_digest);
            }
        };

        let mut upload_blobs = Vec::new();
        let mut missing_digests = HashSet::new();
        add_injected_missing_digests(&input_digests, &mut missing_digests)?;

        let digests_and_ttls_iterator = if deduplicate_get_digests_ttl_calls {
            let (fut, reqs, new) = {
                static GET_DIGESTS_TTL_DEDUP: Lazy<Mutex<GetDigestsTtlDeduper>> =
                    Lazy::new(|| Mutex::new(GetDigestsTtlDeduper::default()));

                GetDigestsTtlDeduper::get_ttls(
                    &GET_DIGESTS_TTL_DEDUP,
                    client,
                    *use_case,
                    identity,
                    digest_config,
                    input_digests.iter().copied(),
                )
            };

            tracing::debug!(
                "Requested digests for {}: {:#?}: {} futures, {} newly dispatched digests",
                input_dir.fingerprint(),
                input_digests.len(),
                reqs,
                new
            );

            let input_digests_ttls = fut.await?;

            struct DigestsWithTtlIterator<I> {
                ttls: HashMap<TrackedFileDigest, i64>,
                inner: I,
            }

            impl<'a, I> Iterator for DigestsWithTtlIterator<I>
            where
                I: Iterator<Item = &'a TrackedFileDigest>,
            {
                type Item = buck2_error::Result<(&'a TrackedFileDigest, i64)>;

                fn next(&mut self) -> Option<buck2_error::Result<(&'a TrackedFileDigest, i64)>> {
                    let digest = self.inner.next()?;
                    let digest_ttl = self
                        .ttls
                        .get(digest)
                        .ok_or_else(|| internal_error!("Did not get a TTL for digest: {}", digest));
                    Some(digest_ttl.map(|ttl| (digest, *ttl)))
                }
            }

            Either::Left(DigestsWithTtlIterator {
                ttls: input_digests_ttls,
                inner: input_digests.into_iter(),
            })
        } else {
            let client = client.clone();
            let metadata = use_case.metadata(identity);
            let digests = input_digests.iter().map(|d| d.to_re()).collect();
            let digests_ttl = client.get_digests_ttl(digests, metadata).await;

            let input_digests = input_digests.iter().copied().collect();

            Either::Right(process_get_digest_ttls_response(
                input_digests,
                digests_ttl?,
                digest_config,
            )?)
        };

        tracing::debug!("Got digests for {}", input_dir.fingerprint());

        // Now find the blobs that need to be uploaded
        for digest_with_ttl in digests_and_ttls_iterator {
            let (digest, digest_ttl) = digest_with_ttl?;

            if digest_ttl <= ttl_wanted {
                tracing::debug!(digest=%digest, ttl=digest_ttl, "Mark for upload");

                match blobs.get(digest) {
                    Some(blob) => {
                        upload_blobs.push(InlinedBlobWithDigest {
                            blob: blob.clone().0,
                            digest: digest.to_re(),
                            ..Default::default()
                        });
                    }
                    None => {
                        missing_digests.insert(digest);
                    }
                }
            } else {
                tracing::debug!(digest=%digest, ttl=digest_ttl, "Not uploading");
                let ttl = Duration::seconds(digest_ttl);
                digest.update_expires(now + ttl);
            }
        }

        Ok((upload_blobs, missing_digests))
    }

    pub async fn upload(
        fs: &ProjectRoot,
        client: &RemoteExecutionClient,
        materializer: &Arc<dyn Materializer>,
        dir_path: &ProjectRelativePath,
        input_dir: &ActionImmutableDirectory,
        blobs: &ActionBlobs,
        use_case: RemoteExecutorUseCase,
        identity: Option<&ReActionIdentity<'_>>,
        digest_config: DigestConfig,
        deduplicate_get_digests_ttl_calls: bool,
    ) -> buck2_error::Result<UploadStats> {
        let (mut upload_blobs, mut missing_digests) = Self::find_missing(
            client,
            input_dir,
            blobs,
            &use_case,
            identity,
            digest_config,
            deduplicate_get_digests_ttl_calls,
        )
        .await?;

        if upload_blobs.is_empty() && missing_digests.is_empty() {
            return Ok(UploadStats::default());
        }

        // Find the file paths and directory blobs that need to be uploaded
        let mut upload_files = Vec::new();

        // Track what files should be materialized before we upload.
        let mut paths_to_materialize = Vec::new();

        if !missing_digests.is_empty() {
            let mut upload_file_paths = Vec::new();
            let mut upload_file_digests = Vec::new();

            {
                let mut walk = input_dir.unordered_walk();
                while let Some((path, entry)) = walk.next() {
                    let digest = match entry {
                        DirectoryEntry::Dir(d) => d.as_fingerprinted_dyn().fingerprint(),
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
                            upload_file_paths.push(dir_path.join(path.get()));
                            upload_file_digests.push(digest.to_re());
                        }
                        DirectoryEntry::Leaf(..) => unreachable!(), // TODO: Better representation of this.
                    };
                }
            }

            if missing_digests.remove(input_dir.fingerprint()) {
                upload_blobs.push(directory_to_blob(input_dir.as_fingerprinted_ref()));
            }

            assert!(
                missing_digests.is_empty(),
                "Expected a path to be found for every digest, traversal code is inconsistent. Left with {missing_digests:?}."
            );

            // Get the real path of the files we are going to upload.
            // This needs to be done because we could have A copied to B, and
            // we are asked to upload B. But since we defer local copies until
            // it's actually needed for a local run, B might not have been
            // copied yet (or ever), so we should upload A directly instead.
            let upload_file_paths = materializer
                .get_materialized_file_paths(upload_file_paths)
                .await?;

            for (name, digest) in upload_file_paths.into_iter().zip(upload_file_digests) {
                match name {
                    Ok(name) => {
                        upload_files.push(NamedDigest {
                            name: fs.resolve(&name).as_maybe_relativized_str()?.to_owned(),
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
                        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) =
                            entry.as_ref()
                        {
                            // NOTE: find_missing has negative caching, so when we query to know if an
                            // artifact was uploaded, if it was the result of an action we just ran, it
                            // won't be here. On the flip side, if a digest has been in the CAS for
                            // a very long time, it might have expired.
                            if file.digest.to_re() == digest {
                                if should_error_for_missing_digest(info) {
                                    soft_error!(
                                        "cas_missing_fatal",
                                        buck2_error::buck2_error!(
                                            buck2_error::ErrorTag::Input,
                                            "{} missing (origin: {})",
                                            file.digest,
                                            info.origin.as_display_for_not_found(),
                                        ),
                                        daemon_in_memory_state_is_corrupted: true,
                                        action_cache_is_corrupted: info.origin.guaranteed_by_action_cache()
                                    )?;

                                    return Err(buck2_error::buck2_error!(
                                        buck2_error::ErrorTag::ReCasArtifactExpired,
                                        "Your build requires an artifact that has expired in the RE CAS \
                                        and Buck does not have it. This likely happened because your Buck daemon \
                                        has been online for a long time. This error is currently unrecoverable. \
                                        To proceed, you should restart Buck using `buck2 killall`. \
                                        Debug information: {:#}",
                                        err
                                    ));
                                }

                                soft_error!(
                                    "cas_missing",
                                    buck2_error::buck2_error!(
                                        buck2_error::ErrorTag::Input,
                                        "{} (expires = {}) is missing in the CAS but expected to exist as per: {:#}",
                                        file.digest,
                                        file.digest.expires()?,
                                        err
                                    ),
                                    quiet: true
                                )?;

                                continue;
                            }
                        }

                        return Err(error_for_missing_file(&digest, err));
                    }
                    Err(ArtifactNotMaterializedReason::RequiresMaterialization { path }) => {
                        upload_files.push(NamedDigest {
                            name: fs.resolve(&path).as_maybe_relativized_str()?.to_owned(),
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
                .buck_error_context("Error materializing paths for upload")?;
        }

        // Compute stats of digests we're about to upload so we can report them
        // to the span end event of this stage of execution.
        let stats = {
            let mut stats_by_extension = HashMap::new();
            let mut named_digest_byte_count: u64 = 0;
            for nd in &upload_files {
                // Aggregate metrics by file extension.
                let byte_count: u64 = nd.digest.size_in_bytes.try_into().unwrap_or_default();
                let extension = extract_file_extension(&nd.name);
                let ext_stats: &mut ReUploadMetrics =
                    stats_by_extension.entry(extension).or_default();
                ext_stats.digests_uploaded += 1;
                ext_stats.bytes_uploaded += byte_count;
                named_digest_byte_count += byte_count;
            }
            let blob_byte_count: u64 = upload_blobs
                .iter()
                .map(|blob| {
                    let byte_count: u64 = blob.digest.size_in_bytes.try_into().unwrap_or_default();
                    byte_count
                })
                .sum();

            UploadStats {
                total: ReUploadMetrics {
                    digests_uploaded: (upload_files.len() + upload_blobs.len()) as u64,
                    bytes_uploaded: named_digest_byte_count + blob_byte_count,
                },
                by_extension: stats_by_extension,
            }
        };

        // Upload
        if !upload_files.is_empty() || !upload_blobs.is_empty() {
            with_error_handler(
                "upload",
                client.get_session_id(),
                client.get_raw_re_client()
                    .upload(
                        use_case.metadata(identity),
                        UploadRequest {
                            files_with_digest: Some(upload_files),
                            inlined_blobs_with_digest: Some(upload_blobs),
                            // all find missing checks are done previously
                            // and we can skip them and upload all digests
                            upload_only_missing: false,
                            ..Default::default()
                        },
                    )
                    .await,
            )
            .await
            .map_err(|e| {
                if e.tags().contains(&buck2_error::ErrorTag::ReInvalidArgument) {
                    buck2_error::buck2_error!(
                        buck2_error::ErrorTag::ReInvalidArgument,
                        "RE Upload failed. It looks like you might have modified files while the build \
                        was in progress. Retry your build to proceed. Debug information: {:#}",
                        e
                    )
                } else {
                    e
                }
            })?;
        };

        Ok(stats)
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
    if let Some(age) = info.action_age() {
        age >= Duration::seconds(3600 * 5)
    } else {
        true
    }
}

fn directory_to_blob<'a, D>(d: D) -> InlinedBlobWithDigest
where
    D: ActionFingerprintedDirectoryRef<'a>,
{
    InlinedBlobWithDigest {
        digest: d.as_fingerprinted_dyn().fingerprint().to_re(),
        blob: ReDirectorySerializer::serialize_entries(d.entries()),
        ..Default::default()
    }
}

fn error_for_missing_file(
    digest: &TDigest,
    cause: &ArtifactNotMaterializedReason,
) -> buck2_error::Error {
    buck2_error::buck2_error!(
        buck2_error::ErrorTag::ReInvalidGetCasResponse,
        "Action execution requires artifact `{}` but the materializer did not return a matching \
        file for this path. This error is unrecoverable and you should restart Buck using \
        `buck2 killall`. We would appreciate a bug report. Debug information: {:#}",
        digest,
        cause,
    )
}

/// This is used for tests. We allow an environment variable to be set to report that some digests
/// are _always_ missing if they are required. This lets us test our upload paths more easily.
fn add_injected_missing_digests<'a>(
    input_digests: &HashSet<&'a TrackedFileDigest>,
    missing_digests: &mut HashSet<&'a TrackedFileDigest>,
) -> buck2_error::Result<()> {
    fn convert_digests(val: &str) -> buck2_error::Result<Vec<FileDigest>> {
        val.split(' ')
            .map(|digest| {
                let digest = TDigest::from_str(digest)
                    .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::InvalidDigest))
                    .with_buck_error_context(|| format!("Invalid digest: `{digest}`"))?;
                // This code does not run in a test but it is only used for testing.
                let digest = FileDigest::from_re(&digest, DigestConfig::testing_default())?;
                buck2_error::Ok(digest)
            })
            .collect()
    }

    let ingested_digests = buck2_env!(
        "BUCK2_TEST_INJECTED_MISSING_DIGESTS",
        type=Vec<FileDigest>,
        converter=convert_digests,
        applicability=testing
    )?;
    if let Some(digests) = ingested_digests {
        for d in digests {
            if let Some(i) = input_digests.get(d) {
                missing_digests.insert(i);
            }
        }
    }

    Ok(())
}

fn extract_file_extension(path: &str) -> String {
    let path = Path::new(path);
    match path.extension() {
        Some(ext) => ext.to_string_lossy().to_lowercase(),
        None => "<empty>".to_owned(),
    }
}

#[derive(
    allocative::Allocative,
    Copy,
    Clone,
    Debug,
    dupe::Dupe,
    PartialEq,
    Eq,
    Hash
)]
struct RequestId(u64);

/// Tracks digests that have in-flight calls to RE and dedupes them.
#[derive(Default)]
struct GetDigestsTtlDeduper<'s> {
    /// Used to allow `digests` to index into `queries`.
    next_request_id: u64,
    /// Maps a given digest to a request that will produce this digest (and
    /// possibly / likely others). The request is referenced as an ID that
    /// can be used to lookup in `queries`.
    digests: HashMap<TrackedFileDigest, RequestId>,
    /// Maps a request to the actual future that will contain its results.
    queries: HashMap<
        RequestId,
        Shared<BoxFuture<'s, buck2_error::Result<HashMap<TrackedFileDigest, i64>>>>,
    >,
}

impl<'s> GetDigestsTtlDeduper<'s> {
    /// Obtain a future that will return the TTLs for the digests that are
    /// queried (and possibly more TTLs).
    fn get_ttls<'a>(
        deduper: &'s Mutex<Self>,
        client: &'a RemoteExecutionClient,
        use_case: RemoteExecutorUseCase,
        identity: Option<&'a ReActionIdentity<'a>>,
        digest_config: DigestConfig,
        digests: impl IntoIterator<Item = &'a TrackedFileDigest>,
    ) -> (
        impl Future<Output = buck2_error::Result<HashMap<TrackedFileDigest, i64>>> + 's,
        usize,
        usize,
    ) {
        let mut guard = deduper.lock().expect("Poisoned lock");

        let mut reqs = HashSet::new();

        let mut to_schedule = Vec::new();

        for digest in digests {
            if let Some(req_id) = guard.digests.get(digest) {
                reqs.insert(*req_id);
            } else {
                to_schedule.push(digest.dupe());
            }
        }

        let to_schedule_len = to_schedule.len();

        if !to_schedule.is_empty() {
            let request_id = RequestId(guard.next_request_id);
            guard.next_request_id += 1;

            reqs.insert(request_id);

            for digest in &to_schedule {
                guard.digests.insert(digest.dupe(), request_id);
            }

            guard.queries.insert(
                request_id,
                query_digest_ttls(
                    deduper,
                    request_id,
                    client,
                    use_case,
                    identity,
                    digest_config,
                    to_schedule,
                )
                .shared(),
            );
        }

        let reqs_len = reqs.len();

        let futs = reqs
            .into_iter()
            .map(|req| guard.queries.get(&req).unwrap().clone())
            .collect::<FuturesUnordered<_>>();

        let fut = async move {
            let results: Vec<_> = futs.try_collect().await?;
            Ok(results.into_iter().flatten().collect())
        };

        (fut, reqs_len, to_schedule_len)
    }
}

/// Call RE, get  the TTLs, then match them back to our inputs. Also deregister
/// this request once it finishes so we don't cache it forever.
fn query_digest_ttls<'s>(
    deduper: &'s Mutex<GetDigestsTtlDeduper>,
    request_id: RequestId,
    client: &RemoteExecutionClient,
    use_case: RemoteExecutorUseCase,
    identity: Option<&ReActionIdentity<'_>>,
    digest_config: DigestConfig,
    input_digests: Vec<TrackedFileDigest>,
) -> BoxFuture<'s, buck2_error::Result<HashMap<TrackedFileDigest, i64>>> {
    let client = client.dupe();
    let metadata = use_case.metadata(identity);
    let digests = input_digests.iter().map(|d| d.to_re()).collect();

    async move {
        let digests_ttl = client.get_digests_ttl(digests, metadata).await;

        {
            let mut guard = deduper.lock().expect("Poisoned lock");
            guard.queries.remove(&request_id);
            for digest in &input_digests {
                guard.digests.remove(digest);
            }
        }

        // It's possibly a bit of a shame that we deregister this response
        // before we process it here, but in practice we need to draw a line at
        // some point and no matter where we draw it, races where we "just miss"
        // the deduped request will exist, unless we have synchronization
        // between checking and setting digest TTLs, unless we cache digests
        // forever (which right now we don't do because we track TTLs on the
        // digest object itself and not all actions are guaranteed to hold the
        // same instance, but maybe that's something that should be revisited),
        // AND figure out invalidation (because the TTL will change when we
        // upload).
        process_get_digest_ttls_response(input_digests, digests_ttl?, digest_config)?.collect()
    }
    .boxed()
}

fn process_get_digest_ttls_response<T>(
    mut req: Vec<T>,
    res: GetDigestsTtlResponse,
    digest_config: DigestConfig,
) -> buck2_error::Result<impl Iterator<Item = buck2_error::Result<(T, i64)>>>
where
    T: Borrow<TrackedFileDigest> + Ord,
{
    let digest_ttls = res.digests_with_ttl;

    if req.len() != digest_ttls.len() {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::ReInvalidGetCasResponse,
            "Invalid response from get_digests_ttl: expected {}, got {} digests",
            req.len(),
            digest_ttls.len()
        ));
    }

    req.sort();

    let mut digest_ttls = digest_ttls.into_try_map(|d| {
        buck2_error::Ok((
            FileDigest::from_re(&d.digest, digest_config).map_err(buck2_error::Error::from)?,
            d.ttl,
        ))
    })?;
    digest_ttls.sort();

    Ok(req
        .into_iter()
        .zip(digest_ttls)
        .map(|(digest, (matching_digest, digest_ttl))| {
            if *digest.borrow().data() != matching_digest {
                return Err(buck2_error::buck2_error!(
                    buck2_error::ErrorTag::ReInvalidGetCasResponse,
                    "Invalid response from get_digests_ttl"
                ));
            }

            Ok((digest, digest_ttl))
        }))
}
