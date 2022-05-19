//! Starlark Actions API for bxl functions
//!

use derivative::Derivative;
use derive_more::Display;
use gazebo::any::AnyLifetime;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue, StarlarkValue, Trace,
        UnpackValue, Value, ValueLike,
    },
};

use crate::bxl::starlark_defs::{
    context::starlark_async::BxlSafeDiceComputations, BxlError::NoFreeze,
};

#[derive(AnyLifetime, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlActionsCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) async_ctx: &'v BxlSafeDiceComputations<'v>,
}

impl<'v> BxlActionsCtx<'v> {
    pub fn new(async_ctx: &'v BxlSafeDiceComputations<'v>) -> Self {
        Self { async_ctx }
    }
}

impl<'v> StarlarkValue<'v> for BxlActionsCtx<'v> {
    starlark_type!("bxl_actions");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> Freeze for BxlActionsCtx<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(NoFreeze("BxlActionsCtx").into())
    }
}

impl<'v> AllocValue<'v> for BxlActionsCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
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
fn register_context(builder: &mut MethodsBuilder) {}
