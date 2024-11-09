/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::AllocValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueTyped;

#[derive(
    Debug,
    derive_more::Display,
    Allocative,
    NoSerialize,
    ProvidesStaticType
)]
#[display("ResolvedDynamicValue<{}>", self.value)]
pub struct StarlarkResolvedDynamicValue {
    pub(crate) value: FrozenValueTyped<'static, FrozenProviderCollection>,
}

#[starlark_value(type = "ResolvedDynamicValue")]
impl<'v> StarlarkValue<'v> for StarlarkResolvedDynamicValue
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(resolved_dynamic_value_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkResolvedDynamicValue {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_module]
fn resolved_dynamic_value_methods(method: &mut MethodsBuilder) {
    /// Get providers from the resolved dynamic value.
    #[starlark(attribute)]
    fn providers<'v>(
        this: ValueTyped<'v, StarlarkResolvedDynamicValue>,
    ) -> starlark::Result<FrozenValueTyped<'static, FrozenProviderCollection>> {
        Ok(this.value)
    }
}

#[starlark_module]
pub(crate) fn register_resolved_dynamic_value(globals: &mut GlobalsBuilder) {
    const ResolvedDynamicValue: StarlarkValueAsType<StarlarkResolvedDynamicValue> =
        StarlarkValueAsType::new();
}
