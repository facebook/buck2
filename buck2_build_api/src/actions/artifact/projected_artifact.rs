use std::sync::Arc;

use buck2_core::fs::paths::{ForwardRelativePath, ForwardRelativePathBuf};
use derive_more::Display;
use gazebo::prelude::*;

use crate::actions::artifact::BaseArtifactKind;

/// A path within another Artifact.
#[derive(Clone, Debug, Display, Dupe, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[display(fmt = "{}/{}", base, path)]
pub struct ProjectedArtifact {
    base: BaseArtifactKind,
    path: Arc<ForwardRelativePathBuf>,
}

impl ProjectedArtifact {
    pub fn new(base: BaseArtifactKind, path: Arc<ForwardRelativePathBuf>) -> Self {
        Self { base, path }
    }

    pub fn base(&self) -> &BaseArtifactKind {
        &self.base
    }

    pub fn path(&self) -> &ForwardRelativePath {
        &self.path
    }
}
