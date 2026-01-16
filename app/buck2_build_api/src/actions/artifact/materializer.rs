/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Instant;

use async_trait::async_trait;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_build_signals::env::NodeDuration;
use buck2_build_signals::env::WaitingData;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::current_span;
use buck2_events::dispatch::span_async_simple;
use buck2_execute::materialize::materializer::HasMaterializer;
use buck2_util::time_span::TimeSpan;
use dice::DiceComputations;
use dupe::Dupe;

use crate::build_signals::HasBuildSignals;

#[async_trait]
pub trait ArtifactMaterializer {
    /// called to materialized the final set of requested artifacts for the build of a target.
    /// This method will render events in superconsole
    async fn try_materialize_requested_artifact(
        &mut self,
        artifact: &BuildArtifact,
        waiting_data: WaitingData,
        required: bool,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<()>;
}

#[async_trait]
impl ArtifactMaterializer for DiceComputations<'_> {
    async fn try_materialize_requested_artifact(
        &mut self,
        artifact: &BuildArtifact,
        waiting_data: WaitingData,
        required: bool,
        path: ProjectRelativePathBuf,
    ) -> buck2_error::Result<()> {
        let materializer = self.per_transaction_data().get_materializer();
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
                    let duration = Instant::now() - now;

                    signals.final_materialization(
                        artifact.dupe(),
                        NodeDuration {
                            user: duration,
                            total: TimeSpan::from_start_and_duration(now, duration),
                            queue: None,
                        },
                        current_span(),
                        waiting_data,
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
