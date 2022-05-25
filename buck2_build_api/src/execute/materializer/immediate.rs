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
use buck2_core::{
    directory::{unordered_entry_walk, DirectoryEntry},
    fs::{
        paths::AbsPathBuf,
        project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf},
    },
};
use futures::stream::{self, BoxStream, StreamExt};
use gazebo::prelude::*;
use remote_execution::{NamedDigest, NamedDigestWithPermissions};

use crate::{
    actions::{
        artifact::ArtifactValue, artifact_utils::materialize_files, digest::FileDigestToReExt,
        directory::ActionDirectoryMember,
    },
    execute::{
        blocking::BlockingExecutor,
        commands::re::manager::ReConnectionManager,
        materializer::{
            http::{http_client, http_download},
            io::MaterializeTreeStructure,
            ArtifactNotMaterializedReason, CasDownloadInfo, CopiedArtifact, HttpDownloadInfo,
            MaterializationError, Materializer,
        },
    },
};

/// Materializer that materializes everything immediately on declare.
pub struct ImmediateMaterializer {
    project_root: AbsPathBuf,
    re_client_manager: Arc<ReConnectionManager>,
    io_executor: Arc<dyn BlockingExecutor>,
}

impl ImmediateMaterializer {
    pub fn new(
        project_root: AbsPathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        io_executor: Arc<dyn BlockingExecutor>,
    ) -> Self {
        Self {
            project_root,
            re_client_manager,
            io_executor,
        }
    }
}

#[async_trait]
impl Materializer for ImmediateMaterializer {
    async fn declare_copy(
        &self,
        path: &ProjectRelativePath,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        // TODO: display [materializing] in superconsole
        self.io_executor
            .execute_io(box MaterializeTreeStructure {
                path: path.to_owned(),
                entry: value.entry().dupe(),
            })
            .await?;

        self.io_executor
            .execute_io_inline(box || {
                for copied_artifact in srcs {
                    // Make sure `path` is a prefix of `dest`, so we don't
                    // materialize anything outside `path`.
                    copied_artifact.dest.strip_prefix(path)
                        .with_context(|| format!(
                            "declare_copy: artifact at `{}` copies into `{}`. This is a bug in Buck, not a user error.",
                            path,
                            &copied_artifact.dest,
                        ))?;
                    materialize_files(
                        copied_artifact.dest_entry.as_ref(),
                        &self.project_root.join_unnormalized(&copied_artifact.src),
                        &self.project_root.join_unnormalized(&copied_artifact.dest),
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
        re_client.materialize_files(files).await?;
        Ok(())
    }

    async fn declare_http(
        &self,
        path: &ProjectRelativePath,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        http_download(
            &http_client()?,
            &ProjectFilesystem {
                root: self.project_root.clone(),
            },
            path,
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
