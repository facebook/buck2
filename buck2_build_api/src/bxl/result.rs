use anyhow::Context;
use starlark::values::{list::List, Value, ValueLike};
use thiserror::Error;

use crate::{
    actions::artifact::Artifact,
    bxl::starlark_defs::context::build::StarlarkBuildResult,
    deferred::{DeferredAny, DeferredId, DeferredTable},
    interpreter::rule_defs::artifact::ValueAsArtifactLike,
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
    pub(super) fn new<'v>(
        has_print: bool,
        eval_value: Value<'v>,
        deferred: DeferredTable,
    ) -> anyhow::Result<Self> {
        if eval_value.is_none() {
            Ok(Self::None { has_print })
        } else if let Some(build_result) = eval_value.downcast_ref::<StarlarkBuildResult>() {
            // TODO avoid the clone if we can extract it from the heap
            Ok(Self::BuildsArtifacts {
                has_print,
                built: vec![build_result.clone()],
                artifacts: vec![],
                deferred,
            })
        } else if let Some(artifact) = eval_value.as_artifact() {
            let artifact = artifact
                .get_bound()
                .context("artifacts needs to be bound to an action")?;

            Ok(Self::BuildsArtifacts {
                has_print,
                built: vec![],
                artifacts: vec![artifact],
                deferred,
            })
        } else if let Some(build_results_list) = List::from_value(eval_value) {
            let mut built = vec![];
            let mut artifacts = vec![];

            for value in build_results_list.iter() {
                if let Some(built_result) = value.downcast_ref::<StarlarkBuildResult>() {
                    built.push(built_result.clone());
                } else if let Some(artifact) = value.as_artifact() {
                    artifacts.push(
                        artifact
                            .get_bound()
                            .context("artifacts needs to be bound to an action")?,
                    );
                } else {
                    return Err(anyhow::anyhow!(NotAValidReturnType(value.get_type())));
                }
            }

            Ok(Self::BuildsArtifacts {
                has_print,
                built,
                artifacts,
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
