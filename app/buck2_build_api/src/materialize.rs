/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_cli_proto::build_request::Materializations;
use buck2_error::BuckErrorContext;
use dashmap::DashSet;
use dice::DiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use futures::FutureExt;

use crate::actions::artifact::materializer::ArtifactMaterializer;
use crate::artifact_groups::calculation::ArtifactGroupCalculation;
use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::ArtifactGroupValues;

pub async fn materialize_artifact_group(
    ctx: &mut DiceComputations<'_>,
    artifact_group: &ArtifactGroup,
    materialization_context: &MaterializationContext,
) -> buck2_error::Result<ArtifactGroupValues> {
    let values = ctx.ensure_artifact_group(artifact_group).await?;

    if let MaterializationContext::Materialize { force } = materialization_context {
        let queue_tracker = ctx
            .per_transaction_data()
            .get_materialization_queue_tracker();
        let mut artifacts_to_materialize = Vec::new();
        for (artifact, _value) in values.iter() {
            if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
                if !queue_tracker.insert(artifact.dupe()) {
                    // We've already requested this artifact, no use requesting it again.
                    continue;
                }
                artifacts_to_materialize.push(artifact);
            }
        }

        ctx.try_compute_join(artifacts_to_materialize, |ctx, artifact| {
            async move {
                ctx.try_materialize_requested_artifact(artifact, *force)
                    .await
            }
            .boxed()
        })
        .await
        .buck_error_context("Failed to materialize artifacts")?;
    }

    Ok(values)
}

#[derive(Clone, Dupe)]
pub enum MaterializationContext {
    Skip,
    Materialize {
        /// Whether we should force the materialization of requested artifacts, or defer to the
        /// config.
        force: bool,
    },
}

impl From<Materializations> for MaterializationContext {
    fn from(value: Materializations) -> Self {
        match value {
            Materializations::Skip => MaterializationContext::Skip,
            Materializations::Default => MaterializationContext::Materialize { force: false },
            Materializations::Materialize => MaterializationContext::Materialize { force: true },
        }
    }
}

/// This map contains all the artifacts that we enqueued for materialization. This ensures
/// we don't enqueue the same thing more than once. Should be shared across work done
/// in a single DICE transaction.
pub struct MaterializationQueueTrackerHolder(Arc<DashSet<BuildArtifact>>);

pub trait HasMaterializationQueueTracker {
    fn init_materialization_queue_tracker(&mut self);

    fn get_materialization_queue_tracker(&self) -> Arc<DashSet<BuildArtifact>>;
}

impl HasMaterializationQueueTracker for UserComputationData {
    fn init_materialization_queue_tracker(&mut self) {
        self.data
            .set(MaterializationQueueTrackerHolder(Arc::new(DashSet::new())));
    }

    fn get_materialization_queue_tracker(&self) -> Arc<DashSet<BuildArtifact>> {
        self.data
            .get::<MaterializationQueueTrackerHolder>()
            .expect("MaterializationQueueTracker should be set")
            .0
            .dupe()
    }
}
