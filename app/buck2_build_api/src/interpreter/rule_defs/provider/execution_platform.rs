/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use starlark::any::ProvidesStaticType;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(ProvidesStaticType, Debug, NoSerialize, Allocative)]
pub struct StarlarkExecutionPlatformResolution(pub ExecutionPlatformResolution);

starlark_simple_value!(StarlarkExecutionPlatformResolution);

#[starlark_value(type = "execution_platform_resolution")]
impl<'v> StarlarkValue<'v> for StarlarkExecutionPlatformResolution {}

impl Display for StarlarkExecutionPlatformResolution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.0.platform() {
            Ok(p) => write!(f, "{}", p.id()),
            Err(e) => write!(f, "{}", e),
        }
    }
}
