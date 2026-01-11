/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::AllocValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

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
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

/// The resolved form of a `DynamicValue` containing the actual providers produced by a dynamic action.
///
/// `ResolvedDynamicValue` is automatically created when a `DynamicValue` is passed to a dynamic
/// action's implementation function via `dynattrs.dynamic_value()`. It provides access to the
/// providers that were produced by the originating dynamic action through its `providers` attribute.
///
/// See [`DynamicValue`](../DynamicValue) for more information
/// ```
#[starlark_module]
fn resolved_dynamic_value_methods(method: &mut MethodsBuilder) {
    /// Get providers from the resolved dynamic value.
    ///
    /// # Example
    ///
    /// ```python
    /// def _impl(actions: AnalysisActions, v: ResolvedDynamicValue, out: OutputArtifact):
    ///     # Access providers
    ///     default_info = v.providers[DefaultInfo]
    ///     custom_info = v.providers[MyInfo]
    /// ```
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
