/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;

use allocative::Allocative;
use buck2_build_api::dynamic_value::DynamicValue;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark_map::StarlarkHasher;

#[derive(
    Debug,
    ProvidesStaticType,
    derive_more::Display,
    Allocative,
    NoSerialize
)]
#[display("DynamicValue<{}>", self.dynamic_value)]
pub struct StarlarkDynamicValue {
    pub(crate) dynamic_value: DynamicValue,
}

#[starlark_value(type = "DynamicValue", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkDynamicValue {
    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.dynamic_value.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        let Some(other) = ValueTyped::<StarlarkDynamicValue>::new(other) else {
            return Ok(false);
        };
        Ok(self.dynamic_value == other.dynamic_value)
    }
}

impl<'v> AllocValue<'v> for StarlarkDynamicValue {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_simple(self)
    }
}

#[starlark_module]
pub(crate) fn register_dynamic_value(globals: &mut GlobalsBuilder) {
    const DynamicValue: StarlarkValueAsType<StarlarkDynamicValue> = StarlarkValueAsType::new();
}
