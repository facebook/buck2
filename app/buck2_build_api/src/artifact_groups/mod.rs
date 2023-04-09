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
pub mod registry;

use allocative::Allocative;
pub use artifact_group_values::ArtifactGroupValues;
use derive_more::Display;
use dupe::Dupe;
use gazebo::variants::UnpackVariants;

use crate::actions::artifact::artifact_type::Artifact;
use crate::artifact_groups::deferred::TransitiveSetKey;

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
}

#[derive(Clone, Debug, Display, Dupe, PartialEq, Eq, Hash, Allocative)]
#[display(fmt = "TransitiveSetProjection({}, {})", key, projection)]
pub struct TransitiveSetProjectionKey {
    pub key: TransitiveSetKey,
    pub projection: usize,
}
