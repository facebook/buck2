/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::bxl::starlark_defs::build_result::StarlarkBxlBuildResult;
use crate::bxl::starlark_defs::context::actions::BxlActions;
use crate::bxl::starlark_defs::context::fs::BxlFilesystem;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

#[starlark_module]
pub(crate) fn register_bxl_type_names(globals: &mut GlobalsBuilder) {
    // TODO(nga): remove these.
    const BxlContext: StarlarkValueAsType<BxlContext> = StarlarkValueAsType::new();
    const BxlActions: StarlarkValueAsType<BxlActions> = StarlarkValueAsType::new();
    const BxlFilesystem: StarlarkValueAsType<BxlFilesystem> = StarlarkValueAsType::new();
    const BxlBuildResult: StarlarkValueAsType<StarlarkBxlBuildResult> = StarlarkValueAsType::new();
}

#[starlark_module]
pub(crate) fn register_bxl_type_names_in_bxl_namespace(globals: &mut GlobalsBuilder) {
    const Context: StarlarkValueAsType<BxlContext> = StarlarkValueAsType::new();
    const Actions: StarlarkValueAsType<BxlActions> = StarlarkValueAsType::new();
    const Filesystem: StarlarkValueAsType<BxlFilesystem> = StarlarkValueAsType::new();
    const BuildResult: StarlarkValueAsType<StarlarkBxlBuildResult> = StarlarkValueAsType::new();
    const TargetSet: StarlarkValueAsType<StarlarkTargetSet<TargetNode>> =
        StarlarkValueAsType::new();
    const ConfiguredTargetSet: StarlarkValueAsType<StarlarkTargetSet<ConfiguredTargetNode>> =
        StarlarkValueAsType::new();
}
