/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::configuration::data::ConfigurationData;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::starlark_value;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct StarlarkConfiguration(pub ConfigurationData);

starlark_simple_value!(StarlarkConfiguration);

#[starlark_value(type = "Configuration")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguration {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configuration_methods)
    }
}

#[starlark_module]
fn configuration_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn name<'v>(this: &'v StarlarkConfiguration) -> starlark::Result<&'v str> {
        Ok(this.0.short_name())
    }

    #[starlark(attribute)]
    fn hash<'v>(this: &'v StarlarkConfiguration) -> starlark::Result<&'v str> {
        Ok(this.0.output_hash().as_str())
    }
}
