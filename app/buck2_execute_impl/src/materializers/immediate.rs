/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_common::http::counting_client::CountingHttpClient;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::CasDigestToReExt;
use buck2_execute::digest_config::DigestConfig;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::execute::clean_output_paths::cleanup_path;
use buck2_execute::execute::clean_output_paths::CleanOutputPaths;
use buck2_execute::materialize::http::http_download;
use buck2_execute::materialize::materializer::ArtifactNotMaterializedReason;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_execute::materialize::materializer::DeclareMatchOutcome;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::re::manager::ReConnectionManager;
use dupe::Dupe;
use futures::stream;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use more_futures::cancellation::CancellationContext;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;

use crate::materializers::io::materialize_files;
use crate::materializers::io::MaterializeTreeStructure;

/// Materializer that materializes everything immediately on declare.
#[derive(Allocative)]
pub struct ImmediateMaterializer {
    fs: ProjectRoot,
    digest_config: DigestConfig,
    re_client_manager: Arc<ReConnectionManager>,
    io_executor: Arc<dyn BlockingExecutor>,
    http_client: CountingHttpClient,
}

impl ImmediateMaterializer {
    pub fn new(
        fs: ProjectRoot,
        digest_config: DigestConfig,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
        http_client: CountingHttpClient,
    ) -> Self {
        Self {
            fs,
            digest_config,
            re_client_manager,
            io_executor,
            http_client,
        }
    }
}

#[async_trait]
impl Materializer for ImmediateMaterializer {
    fn name(&self) -> &str {
        "immediate"
    }

    async fn declare_existing(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        // Nothing to do, we don't keep track of state;
        Ok(())
    }

    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
        cancellations: &CancellationContext,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(
                Box::new(CleanOutputPaths {
                    paths: vec![path.to_owned()],
                }),
                cancellations,
            )
            .await?;

        // TODO: display [materializing] in superconsole
        self.io_executor
            .execute_io(
                Box::new(MaterializeTreeStructure {
                    path: path.clone(),
                    entry: value.entry().dupe(),
                }),
                cancellations,
            )
            .await?;

        self.io_executor
            .execute_io_inline(|| {
                for copied_artifact in srcs {
                    // Make sure `path` is a prefix of `dest`, so we don't
                    // materialize anything outside `path`.
                    copied_artifact.dest.strip_prefix(&path)
                        .with_context(|| format!(
                            "declare_copy: artifact at `{}` copies into `{}`. This is a bug in Buck, not a user error.",
                            path,
                            &copied_artifact.dest,
                        ))?;
                    materialize_files(
                        copied_artifact.dest_entry.as_ref(),
                        &self.fs.root().join(&copied_artifact.src),
                        &self.fs.root().join(&copied_artifact.dest),
                    )?;
                }
                Ok(())
            })
            .await
    }

    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
        cancellations: &CancellationContext,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(
                Box::new(CleanOutputPaths {
                    paths: artifacts.map(|(p, _)| p.to_owned()),
                }),
                cancellations,
            )
            .await?;

        for (path, value) in artifacts.iter() {
            self.io_executor
                .execute_io(
                    Box::new(MaterializeTreeStructure {
                        path: path.to_owned(),
                        entry: value.entry().dupe(),
                    }),
                    cancellations,
                )
                .await?;
        }

        let mut files = Vec::new();
        for (path, value) in artifacts.iter() {
            let mut walk = unordered_entry_walk(value.entry().as_ref());
            while let Some((entry_path, entry)) = walk.next() {
                if let DirectoryEntry::Leaf(ActionDirectoryMember::File(m)) = entry {
                    files.push(NamedDigestWithPermissions {
                        named_digest: NamedDigest {
                            digest: m.digest.to_re(),
                            name: self
                                .fs
                                .resolve(&path.join_normalized(entry_path.get())?)
                                .as_maybe_relativized_str()?
                                .to_owned(),
                            ..Default::default()
                        },
                        is_executable: m.is_executable,
                        ..Default::default()
                    });
                }
            }
        }

        let re_conn = self.re_client_manager.get_re_connection();
        let re_client = re_conn.get_client();
        cancellations
            .critical_section(|| re_client.materialize_files(files, info.re_use_case))
            .await?;
        Ok(())
    }

    async fn declare_http(
        &self,
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
        cancellations: &CancellationContext,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(
                Box::new(CleanOutputPaths {
                    paths: vec![path.to_owned()],
                }),
                cancellations,
            )
            .await?;

        http_download(
            &self.http_client,
            &self.fs,
            self.digest_config,
            &path,
            &info.url,
            &info.checksum,
            info.metadata.is_executable,
        )
        .await?;

        Ok(())
    }

    async fn declare_match(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<DeclareMatchOutcome> {
        // This materializer does not keep track of state
        Ok(DeclareMatchOutcome::NotMatch)
    }

    async fn declare_write<'a>(
        &self,
        gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> anyhow::Result<Vec<ArtifactValue>> {
        write_to_disk(&self.fs, self.io_executor.as_ref(), self.digest_config, gen).await
    }

    async fn invalidate_many(&self, _paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<()> {
        // Nothing to do, we don't keep track of anything.
        Ok(())
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        // We materialize on `declare`, so at this point all declared artifacts
        // have already been materialized.
        Ok(stream::iter(artifact_paths.into_iter().map(|_| Ok(()))).boxed())
    }

    async fn try_materialize_final_artifact(
        &self,
        _artifact_path: ProjectRelativePathBuf,
    ) -> anyhow::Result<bool> {
        // As long as it was declared, it was already materialized
        Ok(true)
    }

    async fn get_materialized_file_paths(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>> {
        // We materialize on `declare`, so all declared paths are already
        // materialized. We can simply return them as is.
        Ok(paths.into_map(Ok))
    }
}

pub async fn write_to_disk<'a>(
    fs: &ProjectRoot,
    io_executor: &dyn BlockingExecutor,
    digest_config: DigestConfig,
    gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
) -> anyhow::Result<Vec<ArtifactValue>> {
    io_executor
        .execute_io_inline({
            move || {
                let requests = gen()?;
                let mut values = Vec::with_capacity(requests.len());

                for WriteRequest {
                    path,
                    content,
                    is_executable,
                } in requests
                {
                    let digest = TrackedFileDigest::from_content(
                        &content,
                        digest_config.cas_digest_config(),
                    );
                    cleanup_path(fs, &path)?;
                    fs.write_file(&path, &content, is_executable)?;

                    values.push(ArtifactValue::file(FileMetadata {
                        digest,
                        is_executable,
                    }));
                }

                Ok(values)
            }
        })
        .await
}
