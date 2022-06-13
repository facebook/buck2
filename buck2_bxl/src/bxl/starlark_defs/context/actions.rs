//! Starlark Actions API for bxl functions
//!

use buck2_build_api::{
    analysis::registry::AnalysisRegistry, bxl::execution_platform::EXECUTION_PLATFORM,
    deferred::BaseDeferredKey,
};
use derivative::Derivative;
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_module, starlark_type,
    values::{
        AllocValue, Heap, NoSerialize, StarlarkValue, Trace, UnpackValue, Value, ValueLike,
        ValueTyped,
    },
};
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;

#[derive(Debug, Error)]
enum BxlActionsError {
    #[error(
        "An action registry was already requested via `action_factory()`. Only one action registry is allowed"
    )]
    RegistryAlreadyCreated,
}

#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlActionsCtx<'v> {
    ctx: ValueTyped<'v, BxlContext<'v>>,
}

impl<'v> BxlActionsCtx<'v> {
    pub fn new(ctx: ValueTyped<'v, BxlContext<'v>>) -> Self {
        Self { ctx }
    }
}

impl<'v> StarlarkValue<'v> for BxlActionsCtx<'v> {
    starlark_type!("bxl_actions");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> AllocValue<'v> for BxlActionsCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> UnpackValue<'v> for &'v BxlActionsCtx<'v> {
    fn expected() -> String {
        BxlActionsCtx::get_type_value_static().as_str().to_owned()
    }

    fn unpack_value(x: Value<'v>) -> Option<&'v BxlActionsCtx<'v>> {
        x.downcast_ref()
    }
}

#[starlark_module]
fn register_context(builder: &mut MethodsBuilder) {
    /// Returns the actions context [`AnalysisRegistry`] to create and register actions for this
    /// bxl function. This will have the same functionality as the actions for rules.
    ///
    /// Actions created by bxl will not be built by default. Instead, they are marked to be built
    /// by `ctx.output.ensure(artifact)` on the output module of the [`BxlContext`]. Only artifacts
    /// marked by ensure will be built.
    fn action_factory<'v>(this: &BxlActionsCtx<'v>) -> anyhow::Result<Value<'v>> {
        let mut registry = this.ctx.as_ref().state.state.borrow_mut();
        if (*registry).is_some() {
            return Err(anyhow::anyhow!(BxlActionsError::RegistryAlreadyCreated));
        } else {
            let analysis_registry = AnalysisRegistry::new_from_owner(
                BaseDeferredKey::BxlLabel(this.ctx.current_bxl.dupe()),
                EXECUTION_PLATFORM.dupe(),
            );

            *registry = Some(analysis_registry);
        }

        Ok(this.ctx.as_ref().state.to_value())
    }
}
