use std::collections::HashSet;

use itertools::Itertools;

use crate::{
    actions::artifact::Artifact,
    bxl::build_result::BxlBuildResult,
    deferred::{DeferredAny, DeferredId, DeferredTable},
    path::BuckOutPath,
};

/// The result of evaluating a bxl function
pub enum BxlResult {
    /// represents that the bxl function has no built results
    None {
        output_loc: BuckOutPath,
        has_print: bool,
    },
    /// a bxl that deals with builds
    BuildsArtifacts {
        output_loc: BuckOutPath,
        has_print: bool,
        built: Vec<BxlBuildResult>,
        artifacts: Vec<Artifact>,
        deferred: DeferredTable,
    },
}

impl BxlResult {
    pub fn new(
        output_loc: BuckOutPath,
        has_print: bool,
        ensured_artifacts: HashSet<Artifact>,
        deferred: DeferredTable,
    ) -> Self {
        if ensured_artifacts.is_empty() {
            Self::None {
                output_loc,
                has_print,
            }
        } else {
            Self::BuildsArtifacts {
                output_loc,
                has_print,
                built: vec![],
                artifacts: ensured_artifacts.into_iter().sorted().collect(),
                deferred,
            }
        }
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        match self {
            BxlResult::None { .. } => Err(anyhow::anyhow!("Bxl never attempted to build anything")),
            BxlResult::BuildsArtifacts { deferred, .. } => deferred.lookup_deferred(id),
        }
    }

    pub fn has_print(&self) -> bool {
        *match self {
            BxlResult::None { has_print, .. } => has_print,
            BxlResult::BuildsArtifacts { has_print, .. } => has_print,
        }
    }

    pub fn get_output_loc(&self) -> &BuckOutPath {
        match self {
            BxlResult::None { output_loc, .. } => output_loc,
            BxlResult::BuildsArtifacts { output_loc, .. } => output_loc,
        }
    }
}
