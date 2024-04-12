/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark globals for BXL.

use buck2_interpreter::link_buck2_downstream_crate_starlark_globals;
use starlark::environment::GlobalsBuilder;

use crate::bxl::starlark_defs::bxl_function::register_bxl_function;
use crate::bxl::starlark_defs::cli_args;
use crate::bxl::starlark_defs::functions::register_artifact_function;
use crate::bxl::starlark_defs::functions::register_error_handling_function;
use crate::bxl::starlark_defs::functions::register_file_set_function;
use crate::bxl::starlark_defs::functions::register_instant_function;
use crate::bxl::starlark_defs::functions::register_target_function;
use crate::bxl::starlark_defs::type_names::register_bxl_type_names;
use crate::bxl::starlark_defs::type_names::register_bxl_type_names_in_bxl_namespace;

fn bxl_namespace(g: &mut GlobalsBuilder) {
    g.struct_("cli_args", cli_args::register_cli_args_module);
    // TODO(nga): add `main` function here.
    register_artifact_function(g);
    register_target_function(g);
    register_file_set_function(g);
    register_instant_function(g);
    register_error_handling_function(g);
    register_bxl_type_names_in_bxl_namespace(g);
}

fn register_bxl_specific_globals(globals: &mut GlobalsBuilder) {
    globals.struct_("bxl", bxl_namespace);
    // TODO(nga): move these into `bxl` namespace.
    globals.struct_("cli_args", cli_args::register_cli_args_module);
    register_bxl_function(globals);
    register_artifact_function(globals);
    register_target_function(globals);
    register_file_set_function(globals);
    register_instant_function(globals);
    register_error_handling_function(globals);
    register_bxl_type_names(globals);
}

link_buck2_downstream_crate_starlark_globals!(register_bxl_specific_globals);
