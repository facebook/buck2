/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::str::FromStr;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_core::buck2_env;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_directory::directory::directory::Directory;
use buck2_directory::directory::directory_iterator::DirectoryIterator;
use buck2_directory::directory::directory_iterator::DirectoryIteratorPathStack;
use buck2_directory::directory::entry::DirectoryEntry;
use buck2_directory::directory::walk::unordered_entry_walk;
use buck2_error::BuckErrorContext;
use buck2_error::ErrorTag;
use buck2_error::conversion::from_any_with_tag;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestFromReExt;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::IoRequest;
use buck2_execute::execute::clean_output_paths::cleanup_path;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::materializer::CasNotFoundError;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::output_size::OutputSize;
use buck2_execute::re::error::RemoteExecutionError;
use buck2_execute::re::manager::ReConnectionManager;
use buck2_fs::error::IoResultExt;
use buck2_fs::fs_util;
use buck2_fs::fs_util::ReadDir;
use buck2_fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_http::HttpClient;
use chrono::Duration;
use chrono::Utc;
use dice_futures::cancellation::CancellationContext;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::TryFutureExt;
use gazebo::prelude::VecExt;
use once_cell::sync::Lazy;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::TCode;
use remote_execution::TDigest;
use tracing::instrument;

use crate::materializers::deferred::ArtifactMaterializationMethod;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::LowPriorityMaterializerCommand;
use crate::materializers::deferred::MaterializeEntryError;
use crate::materializers::deferred::MaterializerSender;
use crate::materializers::deferred::SharedMaterializingError;
use crate::materializers::deferred::Version;
use crate::materializers::deferred::WriteFile;
use crate::materializers::deferred::artifact_tree::MaterializationMethodToProto;
use crate::materializers::deferred::clean_stale::CleanInvalidatedPathRequest;
use crate::materializers::immediate;
use crate::materializers::io::MaterializeTreeStructure;
use crate::materializers::io::materialize_files;

#[derive(Allocative)]
pub struct DefaultIoHandler {
    fs: ProjectRoot,
    digest_config: DigestConfig,
    buck_out_path: ProjectRelativePathBuf,
    re_client_manager: Arc<ReConnectionManager>,
    /// Executor for blocking IO operations
    io_executor: Arc<dyn BlockingExecutor>,
    http_client: HttpClient,
}

struct MaterializationStat {
    file_count: u64,
    total_bytes: u64,
}

#[async_trait]
pub trait IoHandler: Sized + Sync + Send + 'static {
    fn write<'a>(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        write: Arc<WriteFile>,
        version: Version,
        command_sender: Arc<MaterializerSender<Self>>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, Result<(), SharedMaterializingError>>;

    async fn immediate_write<'a>(
        self: &Arc<Self>,
        generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> buck2_error::Result<Vec<ArtifactValue>>;

    fn clean_path<'a>(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        version: Version,
        command_sender: Arc<MaterializerSender<Self>>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, Result<(), buck2_error::Error>>;

    async fn clean_invalidated_path<'a>(
        self: &Arc<Self>,
        request: CleanInvalidatedPathRequest,
        cancellations: &'a CancellationContext,
    ) -> buck2_error::Result<()>;

    async fn materialize_entry(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        event_dispatcher: EventDispatcher,
        cancellations: &CancellationContext,
    ) -> Result<(), MaterializeEntryError>;

    fn create_ttl_refresh(
        self: &Arc<Self>,
        tree: &ArtifactTree,
        min_ttl: Duration,
    ) -> Option<BoxFuture<'static, buck2_error::Result<()>>>;

    fn read_dir(&self, path: &AbsNormPathBuf) -> buck2_error::Result<ReadDir>;
    fn buck_out_path(&self) -> &ProjectRelativePathBuf;
    fn re_client_manager(&self) -> &Arc<ReConnectionManager>;
    fn fs(&self) -> &ProjectRoot;
    fn digest_config(&self) -> DigestConfig;
}

impl DefaultIoHandler {
    pub fn new(
        fs: ProjectRoot,
        digest_config: DigestConfig,
        buck_out_path: ProjectRelativePathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
        http_client: HttpClient,
    ) -> Self {
        Self {
            fs,
            digest_config,
            buck_out_path,
            re_client_manager,
            io_executor,
            http_client,
        }
    }
    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self, stat, cancellations), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry_span(
        &self,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        stat: &mut MaterializationStat,
        cancellations: &CancellationContext,
    ) -> Result<(), MaterializeEntryError> {
        // Materialize the dir structure, and symlinks
        self.io_executor
            .execute_io(
                Box::new(MaterializeTreeStructure {
                    path: path.clone(),
                    entry: entry.dupe(),
                }),
                cancellations,
            )
            .await?;

        // Materialize files
        match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let mut files = Vec::new();

                {
                    let mut walk = unordered_entry_walk(entry.as_ref().map_dir(Directory::as_ref));

                    while let Some((entry_path, entry)) = walk.next() {
                        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) = entry {
                            let name = path.join(entry_path.get());
                            let digest = maybe_tombstone_digest(f.digest.data())?.to_re();

                            tracing::trace!(name = %name, digest = %digest, "push download");
                            let name = self
                                .fs
                                .resolve(&name)
                                .as_maybe_relativized_str()?
                                .to_owned();

                            files.push(NamedDigestWithPermissions {
                                named_digest: NamedDigest {
                                    name,
                                    digest,
                                    ..Default::default()
                                },
                                is_executable: f.is_executable,
                                ..Default::default()
                            });
                        }
                    }
                }
                stat.file_count = files.len().try_into().unwrap_or_default();
                stat.total_bytes = files
                    .iter()
                    .map(|x| u64::try_from(x.named_digest.digest.size_in_bytes).unwrap_or_default())
                    .sum();

                let connection = self.re_client_manager.get_re_connection();
                let re_client = connection.get_client().with_use_case(info.re_use_case);

                re_client
                    .materialize_files(files)
                    .await
                    .map_err(|e| match e.find_typed_context::<RemoteExecutionError>() {
                        Some(re_error) if re_error.code == TCode::NOT_FOUND => {
                            MaterializeEntryError::NotFound(CasNotFoundError {
                                path: Arc::from(path),
                                info: info.dupe(),
                                directory: entry,
                                error: Arc::from(e),
                            })
                        }
                        _ => MaterializeEntryError::Error(e.context({
                            format!("Error materializing files declared by action: {info}")
                        })),
                    })?;
            }
            ArtifactMaterializationMethod::HttpDownload { info } => {
                async {
                    let downloaded = http_download(
                        &self.http_client,
                        &self.fs,
                        self.digest_config,
                        &path,
                        &info.url,
                        &info.checksum,
                        info.metadata.is_executable,
                    )
                    .await?;

                    // Check that the size we got was the one that we expected. This isn't strictly
                    // speaking necessary here, but since an invalid size would break actions
                    // running on RE, it's a good idea to catch it here when materializing so that
                    // our test suite can surface bugs when downloading things locally.
                    if downloaded.size() != info.metadata.digest.size() {
                        return Err(buck2_error::buck2_error!(
                            ErrorTag::DownloadSizeMismatch,
                            "Downloaded size ({}) does not match expected size ({})",
                            downloaded.size(),
                            info.metadata.digest.size(),
                        ));
                    }
                    stat.file_count = 1;
                    stat.total_bytes = info.metadata.digest.size();
                    Ok(())
                }
                .boxed()
                .await
                .with_buck_error_context(|| {
                    format!(
                        "Error materializing HTTP resource declared by target `{}`",
                        info.owner
                    )
                })?;
            }
            ArtifactMaterializationMethod::LocalCopy(_, copied_artifacts) => {
                self.io_executor
                    .execute_io_inline(|| {
                        for a in copied_artifacts {
                            let count_and_bytes = a.dest_entry.calc_output_count_and_bytes();
                            stat.file_count += count_and_bytes.count;
                            stat.total_bytes += count_and_bytes.bytes;

                            materialize_files(
                                a.dest_entry.as_ref(),
                                &self.fs.root().join(&a.src),
                                &self.fs.root().join(&a.dest),
                                a.executable_bit_override,
                            )?;
                        }
                        Ok(())
                    })
                    .await?;
            }
            ArtifactMaterializationMethod::Write(write) => {
                stat.file_count = 1;
                self.io_executor
                    .execute_io_inline(|| {
                        let data =
                            zstd::bulk::decompress(&write.compressed_data, write.decompressed_size)
                                .buck_error_context("Error decompressing data")?;
                        stat.total_bytes = write.decompressed_size as u64;
                        self.fs.write_file(&path, data, write.is_executable)
                    })
                    .await?;
            }
            #[cfg(test)]
            ArtifactMaterializationMethod::Test => unimplemented!(),
        };
        Ok(())
    }
}

#[async_trait]
impl IoHandler for DefaultIoHandler {
    fn write<'a>(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        write: Arc<WriteFile>,
        version: Version,
        command_sender: Arc<MaterializerSender<Self>>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, Result<(), SharedMaterializingError>> {
        self.io_executor
            .execute_io(
                Box::new(WriteIoRequest {
                    path,
                    write,
                    version,
                    command_sender,
                }),
                cancellations,
            )
            .map_err(SharedMaterializingError::Error)
            .boxed()
    }

    async fn immediate_write<'a>(
        self: &Arc<Self>,
        generate: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> buck2_error::Result<Vec<ArtifactValue>> {
        immediate::write_to_disk(
            self.fs(),
            self.io_executor.as_ref(),
            self.digest_config(),
            generate,
        )
        .await
    }

    fn clean_path<'a>(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        version: Version,
        command_sender: Arc<MaterializerSender<Self>>,
        cancellations: &'a CancellationContext,
    ) -> BoxFuture<'a, Result<(), buck2_error::Error>> {
        self.io_executor
            .execute_io(
                Box::new(CleanIoRequest {
                    path,
                    version,
                    command_sender,
                }),
                cancellations,
            )
            .map(|r| r)
            .boxed()
    }

    /// Used to clean paths that are already invalidated and don't need to notify the materializer
    async fn clean_invalidated_path<'a>(
        self: &Arc<Self>,
        request: CleanInvalidatedPathRequest,
        cancellations: &'a CancellationContext,
    ) -> buck2_error::Result<()> {
        self.io_executor
            .execute_io(Box::new(request), cancellations)
            .await
    }

    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self, cancellations), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        event_dispatcher: EventDispatcher,
        cancellations: &CancellationContext,
    ) -> Result<(), MaterializeEntryError> {
        let action_digest = match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                info.action_digest().map(|digest| digest.to_string())
            }
            _ => None,
        };
        let materialization_start = buck2_data::MaterializationStart {
            action_digest: action_digest.clone(),
        };
        event_dispatcher
            .span_async(materialization_start, async move {
                let path_string = path.as_str().to_owned();
                let mut stat = MaterializationStat {
                    file_count: 0,
                    total_bytes: 0,
                };
                let res = self
                    .materialize_entry_span(path, method.dupe(), entry, &mut stat, cancellations)
                    .await;
                let error = res.as_ref().err().map(|e| format!("{e:#}"));

                (
                    res,
                    buck2_data::MaterializationEnd {
                        action_digest,
                        file_count: stat.file_count,
                        total_bytes: stat.total_bytes,
                        path: path_string,
                        success: error.is_none(),
                        error,
                        method: Some(method.to_proto() as i32),
                    },
                )
            })
            .await?;
        Ok(())
    }

    fn create_ttl_refresh(
        self: &Arc<Self>,
        tree: &ArtifactTree,
        min_ttl: Duration,
    ) -> Option<BoxFuture<'static, buck2_error::Result<()>>> {
        create_ttl_refresh(tree, &self.re_client_manager, min_ttl, self.digest_config)
            .map(|f| f.boxed())
    }

    fn read_dir(&self, path: &AbsNormPathBuf) -> buck2_error::Result<ReadDir> {
        fs_util::read_dir(path).categorize_internal()
    }

    fn buck_out_path(&self) -> &ProjectRelativePathBuf {
        &self.buck_out_path
    }

    fn re_client_manager(&self) -> &Arc<ReConnectionManager> {
        &self.re_client_manager
    }

    fn fs(&self) -> &ProjectRoot {
        &self.fs
    }

    fn digest_config(&self) -> DigestConfig {
        self.digest_config
    }
}

/// This is used for testing to ingest digests (via BUCK2_TEST_TOMBSTONED_DIGESTS).
fn maybe_tombstone_digest(digest: &FileDigest) -> buck2_error::Result<&FileDigest> {
    // This has to be of size 1 since size 0 will result in the RE client just producing an empty
    // instead of a not-found error.
    static TOMBSTONE_DIGEST: Lazy<FileDigest> = Lazy::new(|| FileDigest::new_sha1([0; 20], 1));

    fn convert_digests(val: &str) -> buck2_error::Result<HashSet<FileDigest>> {
        val.split(' ')
            .map(|digest| {
                let digest = TDigest::from_str(digest)
                    .map_err(|e| from_any_with_tag(e, ErrorTag::InvalidDigest))
                    .with_buck_error_context(|| format!("Invalid digest: `{digest}`"))?;
                // This code is only used by E2E tests, so while it's not *a test*, testing_default
                // is an OK choice here.
                let digest = FileDigest::from_re(&digest, DigestConfig::testing_default())?;
                buck2_error::Ok(digest)
            })
            .collect()
    }

    let tombstoned_digests = buck2_env!(
        "BUCK2_TEST_TOMBSTONED_DIGESTS",
        type=HashSet<FileDigest>,
        converter=convert_digests,
        applicability=testing,
    )?;
    if let Some(digests) = tombstoned_digests {
        if digests.contains(digest) {
            return Ok(&*TOMBSTONE_DIGEST);
        }
    }

    Ok(digest)
}

/// Spawn a task to refresh TTLs.
pub(super) fn create_ttl_refresh(
    tree: &ArtifactTree,
    re_manager: &Arc<ReConnectionManager>,
    min_ttl: Duration,
    digest_config: DigestConfig,
) -> Option<impl Future<Output = buck2_error::Result<()>> + use<>> {
    let mut digests_to_refresh = HashMap::<_, HashSet<_>>::new();

    let ttl_deadline = Utc::now() + min_ttl;

    for data in tree.iter_without_paths() {
        if let ArtifactMaterializationStage::Declared {
            entry,
            method,
            persist_full_directory_structure: _,
        } = &data.stage
            && let ArtifactMaterializationMethod::CasDownload { info } = method.as_ref()
        {
            let mut walk = unordered_entry_walk(entry.as_ref().map_dir(Directory::as_ref));
            while let Some((_entry_path, entry)) = walk.next() {
                if let DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) = entry {
                    let needs_refresh = file.digest.expires().unwrap_or_default() < ttl_deadline;
                    tracing::trace!("{} needs_refresh: {}", file, needs_refresh);
                    if needs_refresh {
                        digests_to_refresh
                            .entry(info.re_use_case)
                            .or_default()
                            .insert(file.digest.dupe());
                    }
                }
            }
        }
    }

    if digests_to_refresh.is_empty() {
        return None;
    }

    let re_manager = re_manager.dupe();

    let fut = async move {
        // We need to pick *a number* to not send an unbounded amount of digests here. 500 seems
        // broadly reasonable.
        const REFRESH_CHUNK_SIZE: usize = 500;

        let re_connection = re_manager.get_re_connection();
        let re_client = re_connection.get_client();

        for (use_case, digests_to_refresh) in digests_to_refresh {
            let mut digests_to_refresh = digests_to_refresh.into_iter().collect::<Vec<_>>();
            digests_to_refresh.sort();

            for chunk in digests_to_refresh.as_slice().chunks(REFRESH_CHUNK_SIZE) {
                tracing::debug!("Update {} TTLs", chunk.len());

                let digests_expires = re_client
                    .dupe()
                    .with_use_case(use_case)
                    .get_digest_expirations(chunk.iter().map(|d| d.to_re()).collect())
                    .await?;

                let mut digests_expires = digests_expires.into_try_map(|(digest, expires)| {
                    buck2_error::Ok((FileDigest::from_re(&digest, digest_config)?, expires))
                })?;
                digests_expires.sort();

                if chunk.len() != digests_expires.len() {
                    return Err(buck2_error::buck2_error!(
                        ErrorTag::DigestTtlMismatch,
                        "Invalid response from get_digests_ttl: expected {}, got {} digests",
                        chunk.len(),
                        digests_expires.len()
                    ));
                }

                for (digest, (matching_digest, expires)) in chunk.iter().zip(&digests_expires) {
                    if digest.data() != matching_digest {
                        return Err(buck2_error::buck2_error!(
                            ErrorTag::DigestTtlInvalidResponse,
                            "Invalid response from get_digests_ttl"
                        ));
                    }

                    digest.update_expires(*expires);
                }
            }
        }

        buck2_error::Ok(())
    }
    .map(|res| {
        if let Err(e) = &res {
            tracing::info!("TTL Refresh failed: {:#}", e);
        }

        res
    });

    Some(fut)
}

struct WriteIoRequest {
    path: ProjectRelativePathBuf,
    write: Arc<WriteFile>,
    version: Version,
    command_sender: Arc<MaterializerSender<DefaultIoHandler>>,
}

impl WriteIoRequest {
    fn execute_inner(&self, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
        cleanup_path(project_fs, &self.path)?;
        let data =
            zstd::bulk::decompress(&self.write.compressed_data, self.write.decompressed_size)
                .buck_error_context("Error decompressing data")?;
        project_fs.write_file(&self.path, data, self.write.is_executable)?;
        Ok(())
    }
}

impl IoRequest for WriteIoRequest {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
        // NOTE: No spans here! We should perhaps add one, but this needs to be considered
        // carefully as it's a lot of spans, and we haven't historically emitted those for writes.
        let res = self.execute_inner(project_fs);

        // If the materializer has shut down, we ignore this.
        let _ignored = self.command_sender.send_low_priority(
            LowPriorityMaterializerCommand::MaterializationFinished {
                path: self.path,
                timestamp: Utc::now(),
                version: self.version,
                result: res.dupe().map_err(SharedMaterializingError::Error),
            },
        );

        res
    }
}

struct CleanIoRequest {
    path: ProjectRelativePathBuf,
    version: Version,
    command_sender: Arc<MaterializerSender<DefaultIoHandler>>,
}

impl IoRequest for CleanIoRequest {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> buck2_error::Result<()> {
        // NOTE: No spans here! We should perhaps add one, but this needs to be considered
        // carefully as it's a lot of spans, and we haven't historically emitted those for writes.
        let res = cleanup_path(project_fs, &self.path);

        // If the materializer has shut down, we ignore this.
        let _ignored = self.command_sender.send_low_priority(
            LowPriorityMaterializerCommand::CleanupFinished {
                path: self.path,
                version: self.version,
                result: res.dupe().map_err(SharedMaterializingError::Error),
            },
        );

        res
    }
}
