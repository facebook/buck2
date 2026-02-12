/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

mod artifact_group_values;
pub mod calculation;
pub mod deferred;
pub mod promise;

use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
pub use artifact_group_values::ArtifactGroupValues;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::configuration::data::ConfigurationData;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use static_assertions::assert_eq_size;

use self::calculation::EnsureTransitiveSetProjectionKey;
use crate::actions::calculation::BuildKey;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::promise::PromiseArtifact;
use crate::deferred::calculation::GET_PROMISED_ARTIFACT;

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, Hash, Allocative)]
#[display("{} {}", promise_artifact, has_content_based_path)]
pub struct PromiseArtifactWrapper {
    pub promise_artifact: PromiseArtifact,
    pub has_content_based_path: bool,
}

impl PromiseArtifactWrapper {
    pub fn new(artifact: PromiseArtifact, has_content_based_path: bool) -> Self {
        Self {
            promise_artifact: artifact,
            has_content_based_path,
        }
    }
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, Hash, Allocative)]
#[display("{} {}", key, path_resolution_may_require_artifact_value)]
pub struct TransitiveSetProjectionWrapper {
    pub key: TransitiveSetProjectionKey,
    pub path_resolution_may_require_artifact_value: bool,
    pub is_eligible_for_dedupe: bool,
}

impl TransitiveSetProjectionWrapper {
    pub fn new(
        key: TransitiveSetProjectionKey,
        path_resolution_may_require_artifact_value: bool,
        is_eligible_for_dedupe: bool,
    ) -> Self {
        Self {
            key,
            path_resolution_may_require_artifact_value,
            is_eligible_for_dedupe,
        }
    }
}

/// An [ArtifactGroup] can expand to one or more [Artifact]. Those Artifacts will be made available
/// to Actions when they execute.
#[derive(
    Clone,
    Debug,
    Display,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    UnpackVariants,
    Allocative
)]
pub enum ArtifactGroup {
    Artifact(Artifact),
    TransitiveSetProjection(Arc<TransitiveSetProjectionWrapper>),
    Promise(Arc<PromiseArtifactWrapper>),
}

assert_eq_size!(ArtifactGroup, [usize; 2]);

impl ArtifactGroup {
    /// Gets the resolved artifact group, which is used further downstream to use DICE to get
    /// or compute the actual artifact values. For the `Artifact` variant, we will get the results
    /// via the base or projected artifact key. For the `TransitiveSetProjection` variant, we will
    /// look get the results via the `EnsureTransitiveSetProjectionKey`, which expands the underlying
    /// tset. For the `Promise` variant, we will look up the promised artifact values by getting
    /// the analysis results of the owning anon target's analysis.
    pub async fn resolved_artifact(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<ResolvedArtifactGroup<'_>> {
        Ok(match self {
            ArtifactGroup::Artifact(a) => ResolvedArtifactGroup::Artifact(a.clone()),
            ArtifactGroup::TransitiveSetProjection(a) => {
                ResolvedArtifactGroup::TransitiveSetProjection(&a.key)
            }
            ArtifactGroup::Promise(p) => match p.promise_artifact.get() {
                Some(a) => ResolvedArtifactGroup::Artifact(a.clone()),
                None => {
                    let artifact = (GET_PROMISED_ARTIFACT.get()?)(&p.promise_artifact, ctx).await?;
                    ResolvedArtifactGroup::Artifact(artifact)
                }
            },
        })
    }

    pub fn path_resolution_may_require_artifact_value(&self) -> bool {
        match self {
            ArtifactGroup::Artifact(a) => a.path_resolution_requires_artifact_value(),
            ArtifactGroup::TransitiveSetProjection(a) => {
                a.path_resolution_may_require_artifact_value
            }
            ArtifactGroup::Promise(p) => p.has_content_based_path,
        }
    }

    /// Determines whether or not this artifact group is eligible for deduplication across different
    /// configurations.
    ///
    /// This is true if the underlying artifacts are source artifacts, or if they are content-based.
    /// It is also true if they are configured with a different platform than the target platform of
    /// the current node (e.g. execution deps do not prevent dedupe from taking place, even if they
    /// are configuration-based).
    ///
    /// Note that this does not handle transitions correctly. That is generally okay, since we usually
    /// transition at the binary level and those don't tend to be eligible for dedupe anyway. We could
    /// fix that by looking at the execution platform, but we don't have access to that.
    pub fn is_eligible_for_dedupe(&self, target_platform: Option<&ConfigurationData>) -> bool {
        let is_artifact_group_eligible_for_dedupe = match self {
            ArtifactGroup::Artifact(a) => !a.has_configuration_based_path(),
            ArtifactGroup::TransitiveSetProjection(p) => p.is_eligible_for_dedupe,
            ArtifactGroup::Promise(p) => p.has_content_based_path,
        };

        if is_artifact_group_eligible_for_dedupe {
            return true;
        }

        if let Some(target_platform) = target_platform {
            let artifact_group_owner = match self {
                ArtifactGroup::Artifact(a) => a.owner().expect(
                    "Artifact must have an owner, otherwise it would be eligible for dedupe",
                ),
                ArtifactGroup::TransitiveSetProjection(p) => p.key.key.holder_key().owner(),
                // We have to assume that anonymous targets are not eligible for dedupe unless they are content-based,
                // since they have a hash based on their inputs, which will very likely be different across configurations.
                ArtifactGroup::Promise(_) => return false,
            };

            artifact_group_owner
                .configured_label()
                .is_some_and(|l| l.cfg() != target_platform)
        } else {
            // Not building for a specified target platform, input itself needs to be eligible for dedupe
            false
        }
    }
}

// TODO(@wendyy) if we move PromiseArtifact into ArtifactKind someday, we should probably
// split the Artifact variant into two cases (artifact by ref and by value) to prevent memory
// regressions.
#[derive(Clone)]
pub enum ResolvedArtifactGroup<'a> {
    Artifact(Artifact),
    TransitiveSetProjection(&'a TransitiveSetProjectionKey),
}

pub enum ResolvedArtifactGroupBuildSignalsKey {
    EnsureTransitiveSetProjectionKey(EnsureTransitiveSetProjectionKey),
    BuildKey(BuildKey),
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, Hash, Allocative)]
#[display("TransitiveSetProjection({}, {})", key, projection)]
pub struct TransitiveSetProjectionKey {
    pub key: TransitiveSetKey,
    pub projection: usize,
}
