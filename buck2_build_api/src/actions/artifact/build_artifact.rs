use std::{hash::Hash, sync::Arc};

use derivative::Derivative;
use derive_more::Display;
use gazebo::prelude::*;

use crate::{actions::ActionKey, events::proto::ToProtoMessage, path::BuckOutPath};

/// An artifact that is built by the build system
#[allow(clippy::derive_hash_xor_eq)] // The Eq is equivalent to what would have been generated
#[derive(Clone, Debug, Dupe, Display, Hash, Eq, PartialOrd, Ord)]
pub struct BuildArtifact(pub(super) Arc<BuildArtifactData>);

impl PartialEq for BuildArtifact {
    fn eq(&self, other: &Self) -> bool {
        // We know that BuildArtifactData has Eq, and thus is reflexive,
        // so if two values are at the same point, they must be the same.
        // Use that to optimise comparisons.
        //
        // We may be able to state the stronger property that if they
        // aren't the same pointer, they must be different - but hold off
        // on that for now unless it becomes a bottleneck.
        Arc::ptr_eq(&self.0, &other.0) || self.0 == other.0
    }
}

#[derive(Debug, Display, Derivative)]
#[derivative(PartialEq, Eq, Hash, PartialOrd, Ord)]
#[display(fmt = "`{}`, action: {}", path, key)]
pub(super) struct BuildArtifactData {
    pub(super) path: BuckOutPath,
    // If two BuildArtifact's have the same path then they are basically the same,
    // even if the ActionKey differs due to things like dynamic_outputs.
    // TODO(ndmitchell): Clean this up by making it more explicit in ActionKey.
    #[derivative(PartialEq = "ignore")]
    #[derivative(Hash = "ignore")]
    #[derivative(PartialOrd = "ignore")]
    #[derivative(Ord = "ignore")]
    pub(super) key: ActionKey,
}

impl BuildArtifact {
    pub(super) fn new(path: BuckOutPath, key: ActionKey) -> Self {
        Self(Arc::new(BuildArtifactData { path, key }))
    }

    pub fn get_path(&self) -> &BuckOutPath {
        &self.0.path
    }

    pub fn key(&self) -> &ActionKey {
        &self.0.key
    }
}

impl ToProtoMessage for BuildArtifact {
    type Message = buck2_data::BuildArtifact;

    fn as_proto(&self) -> Self::Message {
        buck2_data::BuildArtifact {
            key: Some(self.key().as_proto()),
            path: self.get_path().path().to_string(),
        }
    }
}
