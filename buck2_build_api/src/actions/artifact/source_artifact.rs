use std::{hash::Hash, sync::Arc};

use buck2_core::buck_path::BuckPath;
use derive_more::Display;
use gazebo::prelude::*;

/// An artifact in the source tree
#[derive(Clone, Debug, Display, Dupe, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SourceArtifact(Arc<SourceArtifactData>);

#[derive(Debug, Display, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct SourceArtifactData(BuckPath);

impl SourceArtifact {
    pub fn new(path: BuckPath) -> Self {
        Self(Arc::new(SourceArtifactData(path)))
    }

    pub fn get_path(&self) -> &BuckPath {
        &self.0.0
    }
}
