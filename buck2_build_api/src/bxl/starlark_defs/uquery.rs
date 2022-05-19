use std::sync::Arc;

use derivative::Derivative;
use derive_more::Display;
use gazebo::{any::AnyLifetime, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue, StarlarkValue, Trace,
        UnpackValue, Value, ValueLike,
    },
};

use crate::{
    bxl::starlark_defs::BxlError,
    query::{dice::DiceQueryDelegate, uquery::environment::UqueryEnvironment},
};

#[derive(AnyLifetime, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkUQueryCtx<'v>(
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    UqueryEnvironment<'v>,
);

impl<'v> StarlarkValue<'v> for StarlarkUQueryCtx<'v> {
    starlark_type!("uqueryctx");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_uquery)
    }
}

impl<'v> AllocValue<'v> for StarlarkUQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> Freeze for StarlarkUQueryCtx<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(BxlError::NoFreeze("StarlarkUQueryCtx").into())
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkUQueryCtx<'v> {
    fn expected() -> String {
        StarlarkUQueryCtx::get_type_value_static()
            .as_str()
            .to_owned()
    }

    fn unpack_value(x: Value<'v>) -> Option<&'v StarlarkUQueryCtx<'v>> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkUQueryCtx<'v> {
    pub fn new(cquery_delegate: Arc<DiceQueryDelegate<'v>>) -> anyhow::Result<Self> {
        let env = UqueryEnvironment::new(cquery_delegate.dupe(), cquery_delegate);
        Ok(Self(env))
    }
}

#[starlark_module]
fn register_uquery(builder: &mut MethodsBuilder) {}
