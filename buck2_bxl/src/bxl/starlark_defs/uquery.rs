use std::sync::Arc;

use buck2_build_api::query::dice::DiceQueryDelegate;
use buck2_build_api::query::uquery::environment::UqueryEnvironment;
use derivative::Derivative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;

#[derive(ProvidesStaticType, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkUQueryCtx<'v>(
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    UqueryEnvironment<'v>,
);

impl<'v> StarlarkValue<'v> for StarlarkUQueryCtx<'v> {
    starlark_type!("uqueryctx");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_uquery)
    }
}

impl<'v> AllocValue<'v> for StarlarkUQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkUQueryCtx<'v> {
    fn starlark_type_repr() -> String {
        StarlarkUQueryCtx::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkUQueryCtx<'v> {
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
