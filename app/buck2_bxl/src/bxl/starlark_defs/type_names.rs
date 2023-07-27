/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::actions::BxlActions;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::BxlContext;

#[starlark_module]
pub(crate) fn register_bxl_type_names(globals: &mut GlobalsBuilder) {
    const BxlContext: StarlarkValueAsType<BxlContext> = StarlarkValueAsType::new();
    const BxlActions: StarlarkValueAsType<BxlActions> = StarlarkValueAsType::new();
    const BxlFilesystem: StarlarkValueAsType<BxlFilesystem> = StarlarkValueAsType::new();
    const BxlBuildResult: StarlarkValueAsType<StarlarkBxlBuildResult> = StarlarkValueAsType::new();
}
