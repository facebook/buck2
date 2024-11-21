/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use async_trait::async_trait;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_signals::env::NodeDuration;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::current_span;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::artifact::artifact_dyn::ArtifactDyn;
use buck2_execute::materialize::materializer::HasMaterializer;
use dice::DiceComputations;
use dupe::Dupe;

use crate::actions::artifact::get_artifact_fs::GetArtifactFs;
use crate::build_signals::HasBuildSignals;

#[async_trait]
pub trait ArtifactMaterializer {
    async fn materialize(
        &mut self,
        artifact: &Artifact,
    ) -> buck2_error::Result<ProjectRelativePathBuf>;

    /// called to materialized the final set of requested artifacts for the build of a target.
    /// This method will render events in superconsole
    async fn try_materialize_requested_artifact(
        &mut self,
        artifact: &BuildArtifact,
        required: bool,
    ) -> buck2_error::Result<()>;
}

#[async_trait]
impl ArtifactMaterializer for DiceComputations<'_> {
    async fn materialize(
        &mut self,
        artifact: &Artifact,
    ) -> buck2_error::Result<ProjectRelativePathBuf> {
        let materializer = self.per_transaction_data().get_materializer();
        let artifact_fs = self.get_artifact_fs().await?;
        let path = artifact.resolve_path(&artifact_fs)?;
        materializer.ensure_materialized(vec![path.clone()]).await?;
        Ok(path)
    }

    async fn try_materialize_requested_artifact(
        &mut self,
        artifact: &BuildArtifact,
        required: bool,
    ) -> buck2_error::Result<()> {
        let materializer = self.per_transaction_data().get_materializer();
        let artifact_fs = self.get_artifact_fs().await?;
        let path = artifact_fs.resolve_build(artifact.get_path());

        let start_event = buck2_data::MaterializeRequestedArtifactStart {
            artifact: Some(artifact.as_proto()),
        };

        span_async_simple(
            start_event,
            async move {
                let now = Instant::now();

                let result: buck2_error::Result<_> = try {
                    if required {
                        materializer.ensure_materialized(vec![path]).await?;
                    } else {
                        materializer.try_materialize_final_artifact(path).await?;
                    }
                };

                if let Some(signals) = self.per_transaction_data().get_build_signals() {
                    let duration = now.elapsed();

                    signals.final_materialization(
                        artifact.dupe(),
                        NodeDuration {
                            user: duration,
                            total: duration,
                            queue: None,
                        },
                        current_span(),
                    );
                }

                result
            },
            buck2_data::MaterializeRequestedArtifactEnd {
                artifact: Some(artifact.as_proto()),
            },
        )
        .await
    }
}
