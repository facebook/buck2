/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

mod artifact_group_values;
pub mod calculation;
pub mod deferred;
pub mod promise;

use crate::actions::calculation::BuildKey;
use crate::deferred::calculation::GET_PROMISED_ARTIFACT;

pub mod registry;

use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
pub use artifact_group_values::ArtifactGroupValues;
use buck2_artifact::artifact::artifact_type::Artifact;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;
use static_assertions::assert_eq_size;

use self::calculation::EnsureTransitiveSetProjectionKey;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::promise::PromiseArtifact;

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
    TransitiveSetProjection(Arc<TransitiveSetProjectionKey>),
    Promise(Arc<PromiseArtifact>),
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
    ) -> buck2_error::Result<ResolvedArtifactGroup> {
        Ok(match self {
            ArtifactGroup::Artifact(a) => ResolvedArtifactGroup::Artifact(a.clone()),
            ArtifactGroup::TransitiveSetProjection(a) => {
                ResolvedArtifactGroup::TransitiveSetProjection(a)
            }
            ArtifactGroup::Promise(p) => match p.get() {
                Some(a) => ResolvedArtifactGroup::Artifact(a.clone()),
                None => {
                    let artifact = (GET_PROMISED_ARTIFACT.get()?)(p, ctx).await?;
                    ResolvedArtifactGroup::Artifact(artifact)
                }
            },
        })
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
