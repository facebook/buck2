/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::configuration::Configuration;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkConfiguration(pub Configuration);

starlark_simple_value!(StarlarkConfiguration);

impl<'v> StarlarkValue<'v> for StarlarkConfiguration {
    starlark_type!("configuration");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configuration_methods)
    }
}

#[starlark_module]
fn configuration_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn name(this: &StarlarkConfiguration) -> anyhow::Result<String> {
        Ok(this.0.configuration_platform().to_string())
    }

    #[starlark(attribute)]
    fn hash<'v>(this: &StarlarkConfiguration) -> anyhow::Result<&'v str> {
        Ok(this.0.output_hash())
    }
}
