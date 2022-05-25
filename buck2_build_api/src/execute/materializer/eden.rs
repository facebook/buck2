/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use anyhow::Context as _;
use async_trait::async_trait;
use buck2_core::{
    directory::{DirectoryEntry, FingerprintedDirectory},
    fs::{
        paths::{AbsPathBuf, ForwardRelativePath},
        project::{ProjectRelativePath, ProjectRelativePathBuf},
    },
};
use futures::stream::{self, BoxStream, StreamExt};
use gazebo::prelude::*;
use remote_execution::NamedDigest;

use crate::{
    actions::{
        artifact::ArtifactValue,
        digest::FileDigestToReExt,
        directory::{insert_artifact, ActionDirectoryBuilder, ActionDirectoryMember},
    },
    execute::{
        blocking::BlockingExecutor,
        commands::re::{
            manager::ReConnectionManager, uploader::ActionBlobs, ReExecutorGlobalKnobs,
        },
        materializer::{
            eden_api::EdenBuckOut, immediate::ImmediateMaterializer, ArtifactNotMaterializedReason,
            CasDownloadInfo, CopiedArtifact, HttpDownloadInfo, MaterializationError, Materializer,
        },
    },
};

pub struct EdenMaterializer {
    re_client_manager: Arc<ReConnectionManager>,
    delegator: Arc<dyn Materializer>,
    eden_buck_out: EdenBuckOut,
}

#[async_trait]
impl Materializer for EdenMaterializer {
    /// For Eden, copying could be an expensive operation when large amount of
    /// file system materialization is required. Instead, uploading to CAS then
    /// declare on Eden would be faster so not all tree nodes would be actually materialized.
    async fn declare_copy(
        &self,
        path: &ProjectRelativePath,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        // First upload the src to CAS if missing
        let mut files: Vec<remote_execution::NamedDigest> = Vec::new();
        let mut directories: Vec<remote_execution::Path> = Vec::new();
        for copied_artifact in srcs {
            match copied_artifact.dest_entry {
                DirectoryEntry::Leaf(ActionDirectoryMember::File(file)) => {
                    files.push(NamedDigest {
                        name: copied_artifact.src.to_string(),
                        digest: file.digest.to_re(),
                        ..Default::default()
                    });
                }
                DirectoryEntry::Dir(dir) => {
                    directories.push(remote_execution::Path {
                        path: copied_artifact.src.to_string(),
                        follow_symlinks: false,
                        digest: Some(dir.fingerprint().to_re()),
                        ..Default::default()
                    });
                }
                DirectoryEntry::Leaf(..) => continue,
            };
        }

        self.re_client_manager
            .get_re_connection()
            .get_client()
            .upload_files_and_directories(files, directories, Vec::new(), Default::default())
            .await?;

        // Second upload the tree structure that contains directories/file/symlink metadata
        // TODO(yipu) We don't need to upload CAS, and we should pass ArtifactValue to eden directly
        let path_buf = path.to_buf();
        let mut builder = ActionDirectoryBuilder::empty();
        let path = ForwardRelativePath::new_trim_trailing_slashes(path_buf.as_str())?;
        insert_artifact(&mut builder, path, &value)?;
        let input_dir = builder.fingerprint();

        self.re_client_manager
            .get_re_connection()
            .get_client()
            .upload(
                Arc::clone(&self.delegator),
                &ActionBlobs::new(),
                &input_dir,
                Default::default(),
                &ReExecutorGlobalKnobs {
                    always_check_ttls: true,
                },
            )
            .await?;

        self.eden_buck_out
            .set_path_object_id(&path_buf, &value)
            .await
            .with_context(|| {
                format!(
                    "[eden] Error declaring artifact {:?} at path {}",
                    value, path
                )
            })?;

        Ok(())
    }

    // This method will call Eden's setPathObjectId method, which is to placehold a
    // tree or a blob to a path of an Eden mount.
    async fn declare_cas_many<'a, 'b>(
        &self,
        _info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        let futs = artifacts.iter().map(|(path, value)| async move {
            self.eden_buck_out
                .set_path_object_id(path, value)
                .await
                .with_context(|| {
                    format!(
                        "[eden] Error declaring artifact {:?} at path {}",
                        value, path
                    )
                })
        });

        futures::future::try_join_all(futs).await?;

        Ok(())
    }

    async fn declare_http(
        &self,
        path: &ProjectRelativePath,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        self.delegator.declare_http(path, info).await
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        // EdenFS will handle the on-demand matrialization under the hood, so do nothing here.
        // TODO(yipu): Add options to proactively materialize files on an Eden mount.
        Ok(stream::iter(artifact_paths.into_iter().map(|_| Ok(()))).boxed())
    }

    async fn get_materialized_file_paths(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>> {
        self.delegator.get_materialized_file_paths(paths).await
    }

    async fn try_materialize_final_artifact(
        &self,
        artifact_path: ProjectRelativePathBuf,
    ) -> anyhow::Result<bool> {
        // Similar to ensure_materialized, EdenFS will handle materialization.
        self.ensure_materialized(vec![artifact_path]).await?;
        Ok(true)
    }

    async fn invalidate_many(&self, paths: Vec<ProjectRelativePathBuf>) -> anyhow::Result<()> {
        self.delegator.invalidate_many(paths).await
    }

    fn eden_buck_out(&self) -> Option<&EdenBuckOut> {
        Some(&self.eden_buck_out)
    }
}

impl EdenMaterializer {
    pub fn new(
        project_root: AbsPathBuf,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        eden_buck_out: EdenBuckOut,
    ) -> anyhow::Result<Self> {
        eden_buck_out.setup()?;
        Ok(Self {
            re_client_manager: re_client_manager.dupe(),
            delegator: Arc::new(ImmediateMaterializer::new(
                project_root,
                re_client_manager,
                blocking_executor,
            )),
            eden_buck_out,
        })
    }
}
