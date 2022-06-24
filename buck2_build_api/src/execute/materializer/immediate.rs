/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::directory::unordered_entry_walk;
use buck2_core::directory::DirectoryEntry;
use buck2_core::fs::project::ProjectFilesystem;
use buck2_core::fs::project::ProjectRelativePathBuf;
use futures::stream;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use remote_execution::NamedDigest;
use remote_execution::NamedDigestWithPermissions;

use crate::actions::artifact::ArtifactValue;
use crate::actions::artifact_utils::materialize_files;
use crate::actions::digest::FileDigestToReExt;
use crate::actions::directory::ActionDirectoryMember;
use crate::execute::blocking::BlockingExecutor;
use crate::execute::commands::re::manager::ReConnectionManager;
use crate::execute::materializer::http::http_client;
use crate::execute::materializer::http::http_download;
use crate::execute::materializer::io::MaterializeTreeStructure;
use crate::execute::materializer::ArtifactNotMaterializedReason;
use crate::execute::materializer::CasDownloadInfo;
use crate::execute::materializer::CopiedArtifact;
use crate::execute::materializer::HttpDownloadInfo;
use crate::execute::materializer::MaterializationError;
use crate::execute::materializer::Materializer;
use crate::execute::CleanOutputPaths;

/// Materializer that materializes everything immediately on declare.
pub struct ImmediateMaterializer {
    fs: ProjectFilesystem,
    re_client_manager: Arc<ReConnectionManager>,
    io_executor: Arc<dyn BlockingExecutor>,
}

impl ImmediateMaterializer {
    pub fn new(
        fs: ProjectFilesystem,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
    ) -> Self {
        Self {
            fs,
            re_client_manager,
            io_executor,
        }
    }
}

#[async_trait]
impl Materializer for ImmediateMaterializer {
    async fn declare_copy(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(box CleanOutputPaths {
                paths: vec![path.to_owned()],
            })
            .await?;

        // TODO: display [materializing] in superconsole
        self.io_executor
            .execute_io(box MaterializeTreeStructure {
                path: path.clone(),
                entry: value.entry().dupe(),
            })
            .await?;

        self.io_executor
            .execute_io_inline(box || {
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
                        &self.fs.root.join_unnormalized(&copied_artifact.src),
                        &self.fs.root.join_unnormalized(&copied_artifact.dest),
                    )?;
                }
                Ok(())
            })
            .await
    }

    async fn declare_cas_many<'a, 'b>(
        &self,
        _info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(box CleanOutputPaths {
                paths: artifacts.map(|(p, _)| p.to_owned()),
            })
            .await?;

        for (path, value) in artifacts.iter() {
            self.io_executor
                .execute_io(box MaterializeTreeStructure {
                    path: path.to_owned(),
                    entry: value.entry().dupe(),
                })
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
                            name: path.join_normalized(entry_path.get())?.to_string(),
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
        re_client
            .materialize_files(files, Default::default())
            .await?;
        Ok(())
    }

    async fn declare_http(
        &self,
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        self.io_executor
            .execute_io(box CleanOutputPaths {
                paths: vec![path.to_owned()],
            })
            .await?;

        http_download(
            &http_client()?,
            &self.fs,
            &path,
            &info.url,
            &info.checksum,
            info.metadata.is_executable,
        )
        .await?;

        Ok(())
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
