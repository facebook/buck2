use buck2_execute::path::buck_out_path::BuckOutPath;
use indexmap::IndexSet;
use itertools::Itertools;

use crate::artifact_groups::ArtifactGroup;
use crate::bxl::build_result::BxlBuildResult;
use crate::deferred::types::DeferredAny;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredTable;

/// The result of evaluating a bxl function
pub enum BxlResult {
    /// represents that the bxl function has no built results
    None { output_loc: BuckOutPath },
    /// a bxl that deals with builds
    BuildsArtifacts {
        output_loc: BuckOutPath,
        built: Vec<BxlBuildResult>,
        artifacts: Vec<ArtifactGroup>,
        deferred: DeferredTable,
    },
}

impl BxlResult {
    pub fn new(
        output_loc: BuckOutPath,
        ensured_artifacts: IndexSet<ArtifactGroup>,
        deferred: DeferredTable,
    ) -> Self {
        if ensured_artifacts.is_empty() {
            Self::None { output_loc }
        } else {
            Self::BuildsArtifacts {
                output_loc,
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

    pub fn get_output_loc(&self) -> &BuckOutPath {
        match self {
            BxlResult::None { output_loc, .. } => output_loc,
            BxlResult::BuildsArtifacts { output_loc, .. } => output_loc,
        }
    }
}
