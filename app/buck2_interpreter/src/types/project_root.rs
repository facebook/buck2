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
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::AllocFrozenValue;
use starlark::values::AllocStaticSimple;
use starlark::values::AllocValue;
use starlark::values::FrozenHeap;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkProjectRoot;

#[starlark_value(type = "ProjectRoot", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkProjectRoot {}

fn instance() -> FrozenValue {
    static INSTANCE: AllocStaticSimple<StarlarkProjectRoot> =
        AllocStaticSimple::alloc(StarlarkProjectRoot);
    INSTANCE.to_frozen_value()
}

impl<'v> AllocValue<'v> for StarlarkProjectRoot {
    fn alloc_value(self, _heap: Heap<'v>) -> Value<'v> {
        instance().to_value()
    }
}

impl AllocFrozenValue for StarlarkProjectRoot {
    fn alloc_frozen_value(self, _heap: &FrozenHeap) -> FrozenValue {
        instance()
    }
}

#[starlark_module]
pub fn register_project_root(globals: &mut GlobalsBuilder) {
    const ProjectRoot: StarlarkValueAsType<StarlarkProjectRoot> = StarlarkValueAsType::new();
}
