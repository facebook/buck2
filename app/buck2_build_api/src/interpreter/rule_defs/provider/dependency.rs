/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::mem;

use allocative::Allocative;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProviderName;
use buck2_error::BuckErrorContext;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::typing::Ty;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;

use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::execution_platform::StarlarkExecutionPlatformResolution;
use crate::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum DependencyError {
    #[error("Unknown subtarget, could not find `{0}`")]
    UnknownSubtarget(String),
}

/// Wraps a dependency's `ProvidersLabel` and the result of analysis together for users' rule implementation functions
///
/// From Starlark, the label is accessible with `.label`, and providers from the underlying
/// `ProviderCollection` are available via `[]` (`get()`)
#[derive(
    Debug,
    Trace,
    Coerce,
    Freeze,
    ProvidesStaticType,
    NoSerialize,
    Allocative
)]
#[repr(C)]
pub struct DependencyGen<V: ValueLifetimeless> {
    label: ValueOfUncheckedGeneric<V, StarlarkConfiguredProvidersLabel>,
    provider_collection: FrozenValueTyped<'static, FrozenProviderCollection>,
    // This could be `Option<...>`, but that breaks `Coerce`.
    execution_platform: ValueOfUncheckedGeneric<V, NoneOr<StarlarkExecutionPlatformResolution>>,
}

starlark_complex_value!(pub Dependency);

impl<V: ValueLifetimeless> Display for DependencyGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<dependency ")?;
        Display::fmt(&self.label, f)?;
        write!(f, ">")
    }
}

impl<'v> Dependency<'v> {
    pub fn new(
        heap: &'v Heap,
        label: ConfiguredProvidersLabel,
        provider_collection: FrozenValueTyped<'v, FrozenProviderCollection>,
        execution_platform: Option<&ExecutionPlatformResolution>,
    ) -> Self {
        let execution_platform: ValueOfUnchecked<NoneOr<StarlarkExecutionPlatformResolution>> =
            match execution_platform {
                Some(e) => ValueOfUnchecked::new(
                    heap.alloc(StarlarkExecutionPlatformResolution(e.clone())),
                ),
                None => ValueOfUnchecked::new(Value::new_none()),
            };
        Dependency {
            label: heap.alloc_typed_unchecked(StarlarkConfiguredProvidersLabel::new(label)),
            provider_collection: unsafe {
                mem::transmute::<
                    FrozenValueTyped<'_, FrozenProviderCollection>,
                    FrozenValueTyped<'_, FrozenProviderCollection>,
                >(provider_collection)
            },
            execution_platform,
        }
    }

    pub fn label(&self) -> &StarlarkConfiguredProvidersLabel {
        StarlarkConfiguredProvidersLabel::from_value(self.label.get()).unwrap()
    }

    pub fn execution_platform(&self) -> buck2_error::Result<Option<&ExecutionPlatformResolution>> {
        let execution_platform: ValueOfUnchecked<NoneOr<&StarlarkExecutionPlatformResolution>> =
            self.execution_platform.cast();
        match execution_platform.unpack()? {
            NoneOr::None => Ok(None),
            NoneOr::Other(e) => Ok(Some(&e.0)),
        }
    }
}

#[starlark_value(type = "dependency")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for DependencyGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn get_type_starlark_repr() -> Ty {
        Ty::starlark_value::<DependencyGen<Value<'v>>>()
    }

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(dependency_methods)
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> starlark::Result<Value<'v>> {
        self.provider_collection
            .to_value()
            .at(index, heap)
            .with_buck_error_context(|| format!("Error accessing dependencies of `{}`", self.label))
            .map_err(Into::into)
    }

    fn is_in(&self, other: Value<'v>) -> starlark::Result<bool> {
        self.provider_collection.to_value().is_in(other)
    }
}

/// Dependency type. In Starlark typing it can be represented with `Dependency` global.
#[starlark_module]
fn dependency_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn label<'v>(
        this: &Dependency<'v>,
    ) -> starlark::Result<ValueOfUnchecked<'v, StarlarkConfiguredProvidersLabel>> {
        Ok(this.label)
    }

    // TODO(nga): should return provider collection.
    #[starlark(attribute)]
    fn providers<'v>(this: &Dependency) -> starlark::Result<Vec<FrozenValue>> {
        Ok(this
            .provider_collection
            .providers
            .values()
            .copied()
            .collect())
    }

    /// Obtain the dependency representing a subtarget. In most cases you will want to use
    /// `x[DefaultInfo].sub_targets["foo"]` to get the _providers_ of the subtarget, but if you
    /// need a real `Dependency` type (e.g. for use with `ctx.action.anon_target`) then use
    /// this method.
    fn sub_target<'v>(
        this: &Dependency<'v>,
        #[starlark(require = pos)] subtarget: &str,
        heap: &'v Heap,
    ) -> starlark::Result<Dependency<'v>> {
        let di = this.provider_collection.default_info()?;
        let providers = di.get_sub_target_providers(subtarget).ok_or_else(|| {
            buck2_error::Error::from(DependencyError::UnknownSubtarget(subtarget.to_owned()))
        })?;
        let lbl = StarlarkConfiguredProvidersLabel::from_value(this.label.get())
            .unwrap()
            .inner();
        let lbl = ConfiguredProvidersLabel::new(
            lbl.target().clone(),
            lbl.name().push(ProviderName::new(subtarget.to_owned())?),
        );
        Ok(Dependency::new(heap, lbl, providers, None))
    }

    /// Gets a provider by indexing on a `ProviderCallable` object.
    ///
    /// e.g.
    /// ```ignore
    /// FooInfo = provider(fields=["bar"])
    /// ....
    /// collection.get(FooInfo) # None if absent, a FooInfo instance if present
    /// ```
    fn get<'v>(
        this: &Dependency<'v>,
        index: Value<'v>,
    ) -> starlark::Result<NoneOr<ValueOfUnchecked<'v, AbstractProvider>>> {
        Ok(this
            .provider_collection
            .get(index)
            .with_buck_error_context(|| {
                format!("Error accessing dependencies of `{}`", this.label)
            })?)
    }
}

#[starlark_module]
pub(crate) fn register_dependency(globals: &mut GlobalsBuilder) {
    const Dependency: StarlarkValueAsType<DependencyGen<FrozenValue>> = StarlarkValueAsType::new();
}
