/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_events::dispatch::span_async;
use buck2_execute::materialize::materializer::HasMaterializer;
use dice::DiceComputations;

use crate::actions::artifact::build_artifact::BuildArtifact;
use crate::actions::artifact::Artifact;
use crate::calculation::Calculation;
use crate::events::proto::ToProtoMessage;

#[async_trait]
pub trait ArtifactMaterializer {
    async fn materialize(&self, artifact: &Artifact) -> anyhow::Result<ProjectRelativePathBuf>;

    /// called to materialized the final set of requested artifacts for the build of a target.
    /// This method will render events in superconsole
    async fn try_materialize_requested_artifact(
        &self,
        artifact: &BuildArtifact,
        required: bool,
    ) -> anyhow::Result<()>;
}

#[async_trait]
impl ArtifactMaterializer for DiceComputations {
    async fn materialize(&self, artifact: &Artifact) -> anyhow::Result<ProjectRelativePathBuf> {
        let materializer = self.per_transaction_data().get_materializer();
        let artifact_fs = self.get_artifact_fs().await?;
        let path = artifact_fs.resolve(artifact.get_path())?;
        materializer.ensure_materialized(vec![path.clone()]).await?;
        Ok(path)
    }

    async fn try_materialize_requested_artifact(
        &self,
        artifact: &BuildArtifact,
        required: bool,
    ) -> anyhow::Result<()> {
        let materializer = self.per_transaction_data().get_materializer();
        let artifact_fs = self.get_artifact_fs().await?;
        let path = artifact_fs.resolve_build(artifact.get_path());

        let start_event = buck2_data::MaterializeRequestedArtifactStart {
            artifact: Some(artifact.as_proto()),
        };

        span_async(start_event, async move {
            let result: anyhow::Result<_> = try {
                if required {
                    materializer.ensure_materialized(vec![path]).await?;
                } else {
                    materializer.try_materialize_final_artifact(path).await?;
                }
            };

            (
                result,
                buck2_data::MaterializeRequestedArtifactEnd {
                    artifact: Some(artifact.as_proto()),
                },
            )
        })
        .await
    }
}
