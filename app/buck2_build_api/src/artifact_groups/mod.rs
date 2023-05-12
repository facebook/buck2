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
pub mod registry;

use std::hash::Hash;

use allocative::Allocative;
pub use artifact_group_values::ArtifactGroupValues;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;

use crate::actions::artifact::artifact_type::Artifact;
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
    TransitiveSetProjection(TransitiveSetProjectionKey),
    Promise(PromiseArtifact),
}

impl ArtifactGroup {
    pub fn assert_resolved(&self) -> ResolvedArtifactGroup {
        match self {
            ArtifactGroup::Artifact(a) => ResolvedArtifactGroup::Artifact(a),
            ArtifactGroup::TransitiveSetProjection(a) => {
                ResolvedArtifactGroup::TransitiveSetProjection(a)
            }
            ArtifactGroup::Promise(p) => ResolvedArtifactGroup::Artifact(p.unwrap()),
        }
    }

    pub fn resolved(&self) -> anyhow::Result<ResolvedArtifactGroup> {
        Ok(match self {
            ArtifactGroup::Artifact(a) => ResolvedArtifactGroup::Artifact(a),
            ArtifactGroup::TransitiveSetProjection(a) => {
                ResolvedArtifactGroup::TransitiveSetProjection(a)
            }
            ArtifactGroup::Promise(p) => ResolvedArtifactGroup::Artifact(p.get_err()?),
        })
    }
}
pub enum ResolvedArtifactGroup<'a> {
    Artifact(&'a Artifact),
    TransitiveSetProjection(&'a TransitiveSetProjectionKey),
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, Hash, Allocative)]
#[display(fmt = "TransitiveSetProjection({}, {})", key, projection)]
pub struct TransitiveSetProjectionKey {
    pub key: TransitiveSetKey,
    pub projection: usize,
}
