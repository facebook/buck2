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
use async_trait::async_trait;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_error::buck2_error;
use buck2_futures::cancellation::CancellationContext;
use futures::stream;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use gazebo::prelude::*;

use crate::artifact_value::ArtifactValue;
use crate::materialize::materializer::ArtifactNotMaterializedReason;
use crate::materialize::materializer::CasDownloadInfo;
use crate::materialize::materializer::CopiedArtifact;
use crate::materialize::materializer::DeclareMatchOutcome;
use crate::materialize::materializer::HttpDownloadInfo;
use crate::materialize::materializer::MaterializationError;
use crate::materialize::materializer::Materializer;
use crate::materialize::materializer::WriteRequest;

/// Materializer that doesn't really materialize anything, analogous to
/// materializing to /dev/null. Meant to be used in unittests that need a
/// materializer, but don't want materializations to actually happen.
#[derive(Allocative)]
pub struct NoDiskMaterializer;

#[async_trait]
impl Materializer for NoDiskMaterializer {
    fn name(&self) -> &str {
        "noop"
    }

    async fn declare_existing(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> buck2_error::Result<()> {
        // Nothing to do, we don't keep track of state;
        Ok(())
    }

    async fn declare_copy_impl(
        &self,
        _path: ProjectRelativePathBuf,
        _value: ArtifactValue,
        _srcs: Vec<CopiedArtifact>,
        _cancellations: &CancellationContext,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    async fn declare_cas_many_impl<'a, 'b>(
        &self,
        _info: Arc<CasDownloadInfo>,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
        _cancellations: &CancellationContext,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    async fn declare_http(
        &self,
        _path: ProjectRelativePathBuf,
        _info: HttpDownloadInfo,
        _cancellations: &CancellationContext,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    async fn declare_match(
        &self,
        _artifacts: Vec<(ProjectRelativePathBuf, ArtifactValue)>,
    ) -> buck2_error::Result<DeclareMatchOutcome> {
        // This materializer does not keep track of state
        Ok(DeclareMatchOutcome::NotMatch)
    }

    async fn has_artifact_at(&self, _path: ProjectRelativePathBuf) -> buck2_error::Result<bool> {
        // This materializer does not keep track of state
        Ok(false)
    }

    async fn declare_write<'a>(
        &self,
        _gen: Box<dyn FnOnce() -> buck2_error::Result<Vec<WriteRequest>> + Send + 'a>,
    ) -> buck2_error::Result<Vec<ArtifactValue>> {
        Err(buck2_error!(
            buck2_error::ErrorTag::Input,
            "NoDiskMaterializer cannot write"
        ))
    }

    async fn invalidate_many(
        &self,
        _paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<()> {
        Ok(())
    }

    async fn materialize_many(
        &self,
        artifact_paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<BoxStream<'static, Result<(), MaterializationError>>> {
        Ok(stream::iter(artifact_paths.into_iter().map(|_| Ok(()))).boxed())
    }

    async fn try_materialize_final_artifact(
        &self,
        _artifact_path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<bool> {
        Ok(false)
    }

    async fn get_materialized_file_paths(
        &self,
        paths: Vec<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<Vec<Result<ProjectRelativePathBuf, ArtifactNotMaterializedReason>>>
    {
        Ok(paths.into_map(Ok))
    }
}
