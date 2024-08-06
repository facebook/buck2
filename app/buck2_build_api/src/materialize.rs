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
use buck2_artifact::artifact::artifact_type::BaseArtifactKind;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_cli_proto::build_request::Materializations;
use dashmap::DashSet;
use dice::DiceComputations;
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
) -> anyhow::Result<ArtifactGroupValues> {
    let values = ctx.ensure_artifact_group(artifact_group).await?;

    if let MaterializationContext::Materialize { map, force } = materialization_context {
        let mut artifacts_to_materialize = Vec::new();
        for (artifact, _value) in values.iter() {
            if let BaseArtifactKind::Build(artifact) = artifact.as_parts().0 {
                if !map.insert(artifact.dupe()) {
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
        .context("Failed to materialize artifacts")?;
    }

    Ok(values)
}

#[derive(Clone, Dupe)]
pub enum MaterializationContext {
    Skip,
    Materialize {
        /// This map contains all the artifacts that we enqueued for materialization. This ensures
        /// we don't enqueue the same thing more than once.
        map: Arc<DashSet<BuildArtifact>>,
        /// Whether we should force the materialization of requested artifacts, or defer to the
        /// config.
        force: bool,
    },
}

impl MaterializationContext {
    /// Create a new MaterializationContext that will force all materializations.
    pub fn force_materializations() -> Self {
        Self::Materialize {
            map: Arc::new(DashSet::new()),
            force: true,
        }
    }

    pub fn build_context(behavior: Materializations) -> Self {
        Self::build_context_with_existing_map(behavior, Arc::new(DashSet::new()))
    }

    pub fn build_context_with_existing_map(
        behavior: Materializations,
        map: Arc<DashSet<BuildArtifact>>,
    ) -> Self {
        match behavior {
            Materializations::Skip => MaterializationContext::Skip,
            Materializations::Default => MaterializationContext::Materialize { map, force: false },
            Materializations::Materialize => {
                MaterializationContext::Materialize { map, force: true }
            }
        }
    }
}

/// Contains contexts which share the same materialization map, but might have different behavior.
pub struct MaterializationStrategy {
    /// Context to handle build artifacts
    pub build_context: MaterializationContext,
    /// Context to handle validation artifacts
    pub validation_context: MaterializationContext,
}

impl MaterializationStrategy {
    pub fn new(build_behavior: Materializations) -> Self {
        Self::with_existing_map(build_behavior, &Arc::new(DashSet::new()))
    }

    pub fn with_existing_map(
        build_behavior: Materializations,
        map: &Arc<DashSet<BuildArtifact>>,
    ) -> Self {
        let build_context =
            MaterializationContext::build_context_with_existing_map(build_behavior, map.dupe());
        // Force materialization of validation results as we need to parse
        // them in order to decide whether the build is successful or not.
        let validation_context = MaterializationContext::Materialize {
            map: map.dupe(),
            force: true,
        };
        MaterializationStrategy {
            build_context,
            validation_context,
        }
    }
}
