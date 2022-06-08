use hashbrown::HashSet;
use itertools::Itertools;
use starlark::values::{list::List, Value, ValueLike};
use thiserror::Error;

use crate::{
    actions::artifact::Artifact,
    bxl::starlark_defs::context::build::StarlarkBuildResult,
    deferred::{DeferredAny, DeferredId, DeferredTable},
};

/// The result of evaluating a bxl function
pub enum BxlResult {
    /// represents that the bxl function has no built results
    None { has_print: bool },
    /// a bxl that deals with builds
    BuildsArtifacts {
        has_print: bool,
        built: Vec<StarlarkBuildResult>,
        artifacts: Vec<Artifact>,
        deferred: DeferredTable,
    },
}

#[derive(Debug, Error)]
#[error(
    "Expected `NoneType`, `StarlarkBuildResult`, or a `List` of either `StarlarkBuildResult`, `StarlarkArtifact`, or `StarlarkDeclaredArtifact`  to be returned from bxl. Got return value `{0}`"
)]
struct NotAValidReturnType(&'static str);

impl BxlResult {
    pub(super) fn new(
        has_print: bool,
        ensured_artifacts: HashSet<Artifact>,
        eval_value: Value,
        deferred: DeferredTable,
    ) -> anyhow::Result<Self> {
        if eval_value.is_none() {
            if ensured_artifacts.is_empty() {
                Ok(Self::None { has_print })
            } else {
                Ok(Self::BuildsArtifacts {
                    has_print,
                    built: vec![],
                    artifacts: ensured_artifacts.into_iter().sorted().collect(),
                    deferred,
                })
            }
        } else if let Some(build_result) = eval_value.downcast_ref::<StarlarkBuildResult>() {
            // TODO avoid the clone if we can extract it from the heap
            Ok(Self::BuildsArtifacts {
                has_print,
                built: vec![build_result.clone()],
                artifacts: ensured_artifacts.into_iter().sorted().collect(),
                deferred,
            })
        } else if let Some(build_results_list) = List::from_value(eval_value) {
            let mut built = vec![];

            for value in build_results_list.iter() {
                if let Some(built_result) = value.downcast_ref::<StarlarkBuildResult>() {
                    built.push(built_result.clone());
                } else {
                    return Err(anyhow::anyhow!(NotAValidReturnType(value.get_type())));
                }
            }

            Ok(Self::BuildsArtifacts {
                has_print,
                built,
                artifacts: vec![],
                deferred,
            })
        } else {
            Err(anyhow::anyhow!(NotAValidReturnType(eval_value.get_type())))
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
            BxlResult::None { has_print } => has_print,
            BxlResult::BuildsArtifacts { has_print, .. } => has_print,
        }
    }
}
