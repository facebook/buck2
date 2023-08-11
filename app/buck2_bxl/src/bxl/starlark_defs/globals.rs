/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark globals for BXL.

use buck2_interpreter::bxl::BXL_SPECIFIC_GLOBALS;
use starlark::environment::GlobalsBuilder;

use crate::bxl::starlark_defs::bxl_function::register_bxl_function;
use crate::bxl::starlark_defs::cli_args;
use crate::bxl::starlark_defs::functions::register_artifact_function;
use crate::bxl::starlark_defs::functions::register_error_handling_function;
use crate::bxl::starlark_defs::functions::register_instant_function;
use crate::bxl::starlark_defs::functions::register_label_function;
use crate::bxl::starlark_defs::functions::register_target_function;
use crate::bxl::starlark_defs::type_names::register_bxl_type_names;

fn bxl_namespace(g: &mut GlobalsBuilder) {
    g.struct_("cli_args", cli_args::register_cli_args_module);
    // TODO(nga): add `main` function here.
    register_artifact_function(g);
    register_label_function(g);
    register_target_function(g);
    register_instant_function(g);
    register_error_handling_function(g);
    // TODO(nga): add type constants here.
}

pub(crate) fn init_bxl_specific_globals() {
    BXL_SPECIFIC_GLOBALS.init(|g| {
        g.struct_("bxl", bxl_namespace);
        // TODO(nga): move these into `bxl` namespace.
        g.struct_("cli_args", cli_args::register_cli_args_module);
        register_bxl_function(g);
        register_artifact_function(g);
        register_label_function(g);
        register_target_function(g);
        register_instant_function(g);
        register_error_handling_function(g);
        register_bxl_type_names(g);
    });
}
