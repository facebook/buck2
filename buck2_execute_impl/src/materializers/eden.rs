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
use buck2_common::executor_config::RemoteExecutorUseCase;
use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::FileMetadata;
use buck2_common::file_ops::TrackedFileDigest;
use buck2_core::directory::DirectoryEntry;
use buck2_core::directory::FingerprintedDirectory;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::artifact_value::ArtifactValue;
use buck2_execute::digest::FileDigestToReExt;
use buck2_execute::directory::insert_artifact;
use buck2_execute::directory::ActionDirectoryBuilder;
use buck2_execute::directory::ActionDirectoryMember;
use buck2_execute::execute::blobs::ActionBlobs;
use buck2_execute::execute::blocking::BlockingExecutor;
use buck2_execute::materialize::eden_api::EdenBuckOut;
use buck2_execute::materialize::materializer::ArtifactNotMaterializedReason;
use buck2_execute::materialize::materializer::CasDownloadInfo;
use buck2_execute::materialize::materializer::CopiedArtifact;
use buck2_execute::materialize::materializer::DeclareMatchOutcome;
use buck2_execute::materialize::materializer::HttpDownloadInfo;
use buck2_execute::materialize::materializer::MaterializationError;
use buck2_execute::materialize::materializer::Materializer;
use buck2_execute::materialize::materializer::WriteRequest;
use buck2_execute::re::manager::ReConnectionManager;
use futures::stream;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;
use remote_execution::InlinedBlobWithDigest;
use remote_execution::NamedDigest;

use crate::materializers::immediate::ImmediateMaterializer;

pub struct EdenMaterializer {
    re_client_manager: Arc<ReConnectionManager>,
    delegator: Arc<dyn Materializer>,
    eden_buck_out: EdenBuckOut,
    fs: ProjectRoot,
    re_use_case: RemoteExecutorUseCase,
}

#[async_trait]
impl Materializer for EdenMaterializer {
    async fn declare_existing(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        // Nothing to do, we don't keep track of state;
        Ok(())
    }

    /// For Eden, copying could be an expensive operation when large amount of
    /// file system materialization is required. Instead, uploading to CAS then
    /// declare on Eden would be faster so not all tree nodes would be actually materialized.
    async fn declare_copy_impl(
        &self,
        path: ProjectRelativePathBuf,
        value: ArtifactValue,
        srcs: Vec<CopiedArtifact>,
    ) -> anyhow::Result<()> {
        // Use eden's remove_paths_recursive because it's faster.
        self.eden_buck_out
            .remove_paths_recursive(&self.fs, vec![path.to_owned()])
            .await?;

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
            .upload_files_and_directories(files, directories, Vec::new(), self.re_use_case)
            .await?;

        // Second upload the tree structure that contains directories/file/symlink metadata
        // TODO(yipu) We don't need to upload CAS, and we should pass ArtifactValue to eden directly
        let mut builder = ActionDirectoryBuilder::empty();
        insert_artifact(&mut builder, path.as_ref(), &value)?;
        let input_dir = builder.fingerprint();

        self.re_client_manager
            .get_re_connection()
            .get_client()
            .upload(
                &self.delegator,
                &ActionBlobs::new(),
                ProjectRelativePath::empty(),
                &input_dir,
                self.re_use_case,
            )
            .await?;

        self.eden_buck_out
            .set_path_object_id(&path, &value)
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
    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        _info: Arc<CasDownloadInfo>,
        artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<()> {
        // Use eden's remove_paths_recursive because it's faster.
        self.eden_buck_out
            .remove_paths_recursive(&self.fs, artifacts.map(|(p, _)| p.to_owned()))
            .await?;

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
        path: ProjectRelativePathBuf,
        info: HttpDownloadInfo,
    ) -> anyhow::Result<()> {
        // Use eden's remove_paths_recursive because it's faster.
        self.eden_buck_out
            .remove_paths_recursive(&self.fs, vec![path.to_owned()])
            .await?;

        self.delegator.declare_http(path, info).await
    }

    async fn declare_match(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> anyhow::Result<DeclareMatchOutcome> {
        // This would require querying Eden at a minimum.
        Ok(DeclareMatchOutcome::NotMatch)
    }

    async fn write<'a>(
        &self,
        gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> anyhow::Result<Vec<ArtifactValue>> {
        let (paths, values) =
            write_to_cas(self.re_client_manager.as_ref(), self.re_use_case, gen).await?;

        futures::future::try_join_all(
            std::iter::zip(paths.iter(), values.iter())
                .map(|(path, value)| self.eden_buck_out.set_path_object_id(path, value)),
        )
        .await?;

        Ok(values)
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> anyhow::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        // EdenFS' thrift method ensureMaterialized will force materializing a list of provided paths
        self.eden_buck_out
            .ensure_materialized(artifact_paths.clone())
            .await?;
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
        fs: ProjectRoot,
        re_client_manager: Arc<ReConnectionManager>,
        blocking_executor: Arc<dyn BlockingExecutor>,
        eden_buck_out: EdenBuckOut,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            re_client_manager: re_client_manager.dupe(),
            delegator: Arc::new(ImmediateMaterializer::new(
                fs.dupe(),
                re_client_manager,
                blocking_executor,
            )),
            eden_buck_out,
            fs,
            re_use_case: RemoteExecutorUseCase::buck2_default(), // TODO (yipu): Should this be configurable?
        })
    }
}

async fn write_to_cas<'a>(
    re: &ReConnectionManager,
    re_use_case: RemoteExecutorUseCase,
    gen: Box<dyn FnOnce() -> anyhow::Result<Vec<WriteRequest>> + Send + 'a>,
) -> anyhow::Result<(Vec<ProjectRelativePathBuf>, Vec<ArtifactValue>)> {
    let contents = gen()?;

    let mut uploads = Vec::with_capacity(contents.len());
    let mut paths = Vec::with_capacity(contents.len());
    let mut values = Vec::with_capacity(contents.len());

    for WriteRequest {
        path,
        content,
        is_executable,
    } in contents
    {
        let digest = FileDigest::from_bytes(&content);

        let meta = FileMetadata {
            digest: TrackedFileDigest::new(digest),
            is_executable,
        };

        uploads.push(InlinedBlobWithDigest {
            blob: content,
            digest: meta.digest.to_re(),
            ..Default::default()
        });
        paths.push(path);
        values.push(ArtifactValue::file(meta));
    }

    re.get_re_connection()
        .get_client()
        .upload_files_and_directories(Vec::new(), Vec::new(), uploads, re_use_case)
        .await?;

    Ok((paths, values))
}
