use std::{hash::Hash, sync::Arc};

use derive_more::Display;
use gazebo::prelude::*;

use crate::{events::proto::ToProtoMessage, path::BuckPath};

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

impl ToProtoMessage for SourceArtifact {
    type Message = buck2_data::SourceArtifact;

    fn as_proto(&self) -> Self::Message {
        buck2_data::SourceArtifact {
            path: self.get_path().to_string(),
        }
    }
}
