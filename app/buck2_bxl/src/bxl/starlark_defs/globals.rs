/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark globals for BXL.

use buck2_interpreter::bxl::BxlFunctions;
use buck2_interpreter::bxl::BXL_FUNCTIONS;

use crate::bxl::starlark_defs::bxl_function::register_bxl_function;
use crate::bxl::starlark_defs::cli_args;
use crate::bxl::starlark_defs::functions::register_artifact_function;
use crate::bxl::starlark_defs::functions::register_error_handling_function;
use crate::bxl::starlark_defs::functions::register_instant_function;
use crate::bxl::starlark_defs::functions::register_label_function;
use crate::bxl::starlark_defs::functions::register_target_function;

pub(crate) fn init_bxl_functions() {
    BXL_FUNCTIONS.init(BxlFunctions {
        register_cli_args_struct: |g| g.struct_("cli_args", cli_args::register_cli_args_module),
        register_bxl_function,
        register_artifact_function,
        register_label_function,
        register_target_function,
        register_instant_function,
        register_error_handling_function,
    });
}
