/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkProjectRoot;

impl StarlarkProjectRoot {
    pub fn new() -> Self {
        Self
    }
}

starlark_simple_value!(StarlarkProjectRoot);

#[starlark_value(type = "project_root")]
impl<'v> StarlarkValue<'v> for StarlarkProjectRoot {}

#[starlark_module]
pub fn register_project_root(globals: &mut GlobalsBuilder) {
    const ProjectRoot: StarlarkValueAsType<StarlarkProjectRoot> = StarlarkValueAsType::new();
}
