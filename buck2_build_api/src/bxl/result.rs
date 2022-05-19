use starlark::values::{list::List, Value, ValueLike};
use thiserror::Error;

use crate::{
    bxl::starlark_defs::context::build::StarlarkBuildResult,
    deferred::{DeferredAny, DeferredId, DeferredTable},
};

/// The result of evaluating a bxl function
pub enum BxlResult {
    /// represents that the bxl function has no built results
    None,
    /// a list of built artifacts that the bxl function intends to expose to callers
    Built(Vec<StarlarkBuildResult>, DeferredTable),
}

#[derive(Debug, Error)]
#[error(
    "Expected `NoneType`, `StarlarkBuildResult`, or a `List` of `StarlarkBuildResult` to be returned from bxl. Got return value `{0}`"
)]
struct NotAStarlarkBuildResult(&'static str);

impl BxlResult {
    pub(super) fn new<'v>(eval_value: Value<'v>, deferred: DeferredTable) -> anyhow::Result<Self> {
        if eval_value.is_none() {
            Ok(Self::None)
        } else if let Some(build_result) = eval_value.downcast_ref::<StarlarkBuildResult>() {
            // TODO avoid the clone if we can extract it from the heap
            Ok(Self::Built(vec![build_result.clone()], deferred))
        } else if let Some(build_results_list) = List::from_value(eval_value) {
            Ok(Self::Built(
                build_results_list
                    .iter()
                    .map(|value| {
                        value
                            .downcast_ref::<StarlarkBuildResult>()
                            .ok_or_else(|| {
                                anyhow::anyhow!(NotAStarlarkBuildResult(value.get_type()))
                            })
                            .map(|result| result.clone())
                    })
                    .collect::<anyhow::Result<_>>()?,
                deferred,
            ))
        } else {
            Err(anyhow::anyhow!(NotAStarlarkBuildResult(
                eval_value.get_type()
            )))
        }
    }

    /// looks up an 'Deferred' given the id
    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        match self {
            BxlResult::None => Err(anyhow::anyhow!("Bxl never attempted to build anything")),
            BxlResult::Built(_, deferred) => deferred.lookup_deferred(id),
        }
    }
}
