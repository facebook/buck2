/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::str::FromStr;
use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_common::file_ops::FileDigest;
use buck2_common::result::SharedError;
use buck2_common::result::ToSharedResultExt;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;
use buck2_core::env_helper::EnvHelper;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_events::dispatch::EventDispatcher;
use buck2_execute::digest::CasDigestFromReExt;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::directory::ActionDirectoryEntry;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::directory::ActionSharedDirectory;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::blocking::IoRequest;
use buck2_execute::execute::clean_output_paths::cleanup_path;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::materialize::http::http_client;
use buck2_execute::materialize::http::http_download;
use buck2_execute::output_size::OutputSize;
use buck2_execute::re::manager::ReConnectionManager;
use chrono::Duration;
use chrono::Utc;
use futures::future::BoxFuture;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::TryFutureExt;
use gazebo::prelude::*;
use once_cell::sync::Lazy;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;
use remote_execution::REClientError;
use remote_execution::TCode;
use remote_execution::TDigest;
use tracing::instrument;

use crate::materializers::deferred::ArtifactMaterializationMethod;
use crate::materializers::deferred::ArtifactMaterializationStage;
use crate::materializers::deferred::ArtifactTree;
use crate::materializers::deferred::MaterializationMethodToProto;
use crate::materializers::deferred::MaterializeEntryError;
use crate::materializers::deferred::MaterializerCommand;
use crate::materializers::deferred::MaterializerSender;
use crate::materializers::deferred::SharedMaterializingError;
use crate::materializers::deferred::WriteFile;
use crate::materializers::io::materialize_files;
use crate::materializers::io::MaterializeTreeStructure;

pub(super) struct DefaultIoHandler {
    pub(super) fs: ProjectRoot,
    pub(super) re_client_manager: Arc<ReConnectionManager>,
    /// Executor for blocking IO operations
    pub(super) io_executor: Arc<dyn BlockingExecutor>,
}

struct MaterializationStat {
    file_count: u64,
    total_bytes: u64,
}

#[async_trait]
pub(super) trait IoHandler: Sync + Send + 'static {
    fn write(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        write: Arc<WriteFile>,
        version: u64,
        command_sender: MaterializerSender<Self>,
    ) -> BoxFuture<'static, Result<(), SharedMaterializingError>>;

    fn clean_output_paths(
        self: &Arc<Self>,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> BoxFuture<'static, Result<(), SharedError>>;

    async fn materialize_entry(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        event_dispatcher: EventDispatcher,
    ) -> Result<(), MaterializeEntryError>;

    fn create_ttl_refresh(
        self: &Arc<Self>,
        tree: &ArtifactTree,
        min_ttl: Duration,
    ) -> Option<BoxFuture<'static, anyhow::Result<()>>>;
}

impl DefaultIoHandler {
    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self, stat), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry_span(
        &self,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        stat: &mut MaterializationStat,
    ) -> Result<(), MaterializeEntryError> {
        // Materialize the dir structure, and symlinks
        self.io_executor
            .execute_io(box MaterializeTreeStructure {
                path: path.clone(),
                entry: entry.dupe(),
            })
            .await?;

        // Materialize files
        match method.as_ref() {
            ArtifactMaterializationMethod::CasDownload { info } => {
                let mut files = Vec::new();

                {
                    let mut walk = unordered_entry_walk(entry.as_ref());

                    while let Some((entry_path, entry)) = walk.next() {
                        if let DirectoryEntry::Leaf(ActionDirectoryMember::File(f)) = entry {
                            let name = path.join_normalized(entry_path.get())?.to_string();
                            let digest = maybe_tombstone_digest(f.digest.data())?.to_re();

                            tracing::trace!(name = %name, digest = %digest, "push download");

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
                let re_client = connection.get_client();

                re_client
                    .materialize_files(files, info.re_use_case)
                    .await
                    .map_err(|e| match e.downcast_ref::<REClientError>() {
                        Some(e) if e.code == TCode::NOT_FOUND => {
                            MaterializeEntryError::NotFound { info: info.dupe() }
                        }
                        _ => MaterializeEntryError::Error(e.context({
                            format!("Error materializing files declared by action: {}", info)
                        })),
                    })?;
            }
            ArtifactMaterializationMethod::HttpDownload { info } => {
                async {
                    let downloaded = http_download(
                        &http_client()?,
                        &self.fs,
                        &path,
                        &info.url,
                        &info.checksum,
                        info.metadata.is_executable,
                    )
                    .await?;

                    // Check that the size we got was the one that we expected. This isn't stricly
                    // speaking necessary here, but since an invalid size would break actions
                    // running on RE, it's a good idea to catch it here when materializing so that
                    // our test suite can surface bugs when downloading things locally.
                    if downloaded.size() != info.metadata.digest.size() {
                        return Err(anyhow::anyhow!(
                            "Downloaded size ({}) does not match expected size ({})",
                            downloaded.size(),
                            info.metadata.digest.size(),
                        ));
                    }
                    stat.file_count = 1;
                    stat.total_bytes = info.metadata.digest.size();
                    Ok(())
                }
                .await
                .with_context(|| {
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
                                .context("Error decompressing data")?;
                        stat.total_bytes = write.decompressed_size as u64;
                        self.fs.write_file(&path, &data, write.is_executable)
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
    fn write(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        write: Arc<WriteFile>,
        version: u64,
        command_sender: MaterializerSender<Self>,
    ) -> BoxFuture<'static, Result<(), SharedMaterializingError>> {
        self.io_executor
            .execute_io(box WriteIoRequest {
                path,
                write,
                version,
                command_sender,
            })
            .map_err(|e| SharedMaterializingError::Error(e.into()))
            .boxed()
    }

    fn clean_output_paths(
        self: &Arc<Self>,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> BoxFuture<'static, Result<(), SharedError>> {
        self.io_executor
            .execute_io(box CleanOutputPaths { paths })
            .map(|r| r.shared_error())
            .boxed()
    }

    /// Materializes an `entry` at `path`, using the materialization `method`
    #[instrument(level = "debug", skip(self), fields(path = %path, method = %method, entry = %entry))]
    async fn materialize_entry(
        self: &Arc<Self>,
        path: ProjectRelativePathBuf,
        method: Arc<ArtifactMaterializationMethod>,
        entry: ActionDirectoryEntry<ActionSharedDirectory>,
        event_dispatcher: EventDispatcher,
    ) -> Result<(), MaterializeEntryError> {
        let materialization_start = buck2_data::MaterializationStart {
            action_digest: match method.as_ref() {
                ArtifactMaterializationMethod::CasDownload { info } => {
                    info.action_digest().map(|digest| digest.to_string())
                }
                _ => None,
            },
        };
        event_dispatcher
            .span_async(materialization_start, async {
                let path_string = path.as_str().to_owned();
                let mut stat = MaterializationStat {
                    file_count: 0,
                    total_bytes: 0,
                };
                let res = self
                    .materialize_entry_span(path, method.dupe(), entry, &mut stat)
                    .await;
                let error = res.as_ref().err().map(|e| format!("{:#}", e));

                (
                    res,
                    buck2_data::MaterializationEnd {
                        action_digest: None,
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
    ) -> Option<BoxFuture<'static, anyhow::Result<()>>> {
        create_ttl_refresh(tree, &self.re_client_manager, min_ttl).map(|f| f.boxed())
    }
}

/// This is used for testing to ingest digests (via BUCK2_TEST_TOMBSTONED_DIGESTS).
fn maybe_tombstone_digest(digest: &FileDigest) -> anyhow::Result<&FileDigest> {
    // This has to be of size 1 since size 0 will result in the RE client just producing an empty
    // instead of a not-found error.
    static TOMBSTONE_DIGEST: Lazy<FileDigest> = Lazy::new(|| FileDigest::new_sha1([0; 20], 1));

    fn convert_digests(val: &str) -> anyhow::Result<HashSet<FileDigest>> {
        val.split(' ')
            .map(|digest| {
                let digest = TDigest::from_str(digest)
                    .with_context(|| format!("Invalid digest: `{}`", digest))?;
                let digest = FileDigest::from_re(&digest);
                anyhow::Ok(digest)
            })
            .collect()
    }

    static TOMBSTONED_DIGESTS: EnvHelper<HashSet<FileDigest>> =
        EnvHelper::with_converter("BUCK2_TEST_TOMBSTONED_DIGESTS", convert_digests);

    if let Some(digests) = TOMBSTONED_DIGESTS.get()? {
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
) -> Option<impl Future<Output = anyhow::Result<()>>> {
    let mut digests_to_refresh = HashSet::new();

    let ttl_deadline = Utc::now() + min_ttl;

    for (_, data) in tree.iter() {
        match &data.stage {
            ArtifactMaterializationStage::Declared { method, .. } => match method.as_ref() {
                ArtifactMaterializationMethod::CasDownload { info } => {
                    if let Some(action_digest) = info.action_digest() {
                        if action_digest.expires() <= ttl_deadline {
                            digests_to_refresh.insert((action_digest.dupe(), info.re_use_case));
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    if digests_to_refresh.is_empty() {
        None
    } else {
        let re_manager = re_manager.dupe();

        Some(async move {
            let re_connection = re_manager.get_re_connection();
            let re_client = re_connection.get_client();
            let re_client = &re_client;

            futures::future::join_all(digests_to_refresh.iter().map(
                |(digest, use_case)| async move {
                    // A side effect of action cache queries is to refresh the underlying outputs.
                    match re_client
                        .action_cache(digest.data().dupe(), *use_case)
                        .await
                    {
                        Ok(Some(res)) => {
                            let expires = Utc::now() + Duration::seconds(res.ttl);
                            digest.update_expires(expires);
                            tracing::debug!("Updated expiry for action `{}`: {}", digest, expires)
                        }
                        Ok(None) => {
                            tracing::info!(
                                "Action `{}` is referenced by materializer, but expired",
                                digest
                            );
                        }
                        Err(e) => {
                            tracing::info!(
                                "Failed to query action cache for action `{}`: {:#}",
                                digest,
                                e
                            );
                        }
                    }
                },
            ))
            .await;

            // Currently we don't propagate errors back here.
            Ok(())
        })
    }
}

struct WriteIoRequest {
    path: ProjectRelativePathBuf,
    write: Arc<WriteFile>,
    version: u64,
    command_sender: MaterializerSender<DefaultIoHandler>,
}

impl WriteIoRequest {
    fn execute_inner(&self, project_fs: &ProjectRoot) -> anyhow::Result<()> {
        cleanup_path(project_fs, &self.path)?;
        let data =
            zstd::bulk::decompress(&self.write.compressed_data, self.write.decompressed_size)
                .context("Error decompressing data")?;
        project_fs.write_file(&self.path, &data, self.write.is_executable)?;
        Ok(())
    }
}

impl IoRequest for WriteIoRequest {
    fn execute(self: Box<Self>, project_fs: &ProjectRoot) -> anyhow::Result<()> {
        // NOTE: No spans here! We should perhaps add one, but this needs to be considered
        // carefully as it's a lot of spans, and we haven't historically emitted those for writes.
        let res = self.execute_inner(project_fs).shared_error();

        // If the materializer has shut down, we ignore this.
        let _ignored = self
            .command_sender
            .send(MaterializerCommand::MaterializationFinished {
                path: self.path,
                timestamp: Utc::now(),
                version: self.version,
                result: res.dupe().map_err(SharedMaterializingError::Error),
            });

        Ok(res?)
    }
}
