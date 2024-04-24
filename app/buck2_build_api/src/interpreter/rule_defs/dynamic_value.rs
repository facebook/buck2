/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use anyhow::Context;
use buck2_artifact::deferred::key::DeferredKey;
use buck2_interpreter::error::BuckStarlarkError;
use derive_more::Display;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_simple_value;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::Coerce;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::ProvidesStaticType;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueOfUnchecked;
use starlark_map::StarlarkHasher;

use crate::interpreter::rule_defs::provider::collection::ProviderCollection;
use crate::interpreter::rule_defs::provider::ty::abstract_provider::AbstractProvider;

#[derive(Clone, Debug, Display, ProvidesStaticType, NoSerialize, Allocative)]
#[display(fmt = "dynamic_value({})", dynamic_output_key)]
pub struct DynamicValue {
    pub dynamic_output_key: DeferredKey,
}

starlark_simple_value!(DynamicValue);

#[starlark_value(type = "dynamic_value")]
impl<'v> StarlarkValue<'v> for DynamicValue {
    type Canonical = Self;

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        Hash::hash(self, hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        Ok(other
            .downcast_ref::<Self>()
            .map_or(false, |other| self == other))
    }
}

impl UnpackValue<'_> for DynamicValue {
    fn unpack_value(value: Value) -> Option<Self> {
        Some(value.downcast_ref::<Self>()?.clone())
    }
}

impl PartialEq for DynamicValue {
    fn eq(&self, other: &Self) -> bool {
        self.dynamic_output_key == other.dynamic_output_key
    }
}

impl Eq for DynamicValue {}

impl Hash for DynamicValue {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        Hash::hash(&self.dynamic_output_key, hasher);
    }
}

#[derive(
    Debug,
    Trace,
    Coerce,
    Display,
    Freeze,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[display(fmt = "resolved_dynamic_value")]
#[repr(C)]
pub struct ResolvedDynamicValueGen<V> {
    providers_collection: V,
}

starlark_complex_value!(pub ResolvedDynamicValue);

impl<'v> ResolvedDynamicValue<'v> {
    pub fn new(providers_collection: Value<'v>) -> Self {
        ResolvedDynamicValue {
            providers_collection,
        }
    }

    fn provider_collection(&self) -> anyhow::Result<&ProviderCollection<'v>> {
        ProviderCollection::from_value(self.providers_collection)
            .ok_or_else(|| anyhow::anyhow!("internal error: not a ProviderCollection"))
    }
}

#[starlark_value(type = "resolved_dynamic_value")]
impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for ResolvedDynamicValueGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(resolved_dynamic_value_methods)
    }

    fn at(&self, index: Value<'v>, heap: &'v Heap) -> starlark::Result<Value<'v>> {
        self.providers_collection
            .to_value()
            .at(index, heap)
            .map_err(BuckStarlarkError::new)
            .with_context(|| "Error accessing dynamic value".to_owned())
            .map_err(Into::into)
    }

    fn is_in(&self, other: Value<'v>) -> starlark::Result<bool> {
        self.providers_collection.to_value().is_in(other)
    }
}

#[starlark_module]
fn resolved_dynamic_value_methods(builder: &mut MethodsBuilder) {
    // TODO(nga): should return provider collection.
    #[starlark(attribute)]
    fn providers<'v>(this: &ResolvedDynamicValue) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(this
            .provider_collection()?
            .providers
            .values()
            .copied()
            .collect())
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
        this: &ResolvedDynamicValue<'v>,
        index: Value<'v>,
    ) -> anyhow::Result<NoneOr<ValueOfUnchecked<'v, AbstractProvider>>> {
        this.provider_collection()?
            .get(index)
            .with_context(|| "Error accessing dynamic value".to_owned())
    }
}

#[starlark_module]
pub(crate) fn register_dynamic_value(globals: &mut GlobalsBuilder) {
    const DynamicValue: StarlarkValueAsType<DynamicValue> = StarlarkValueAsType::new();
    const ResolvedDynamicValue: StarlarkValueAsType<ResolvedDynamicValue> =
        StarlarkValueAsType::new();
}
