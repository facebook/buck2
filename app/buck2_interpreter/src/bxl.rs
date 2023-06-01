/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;
use starlark::environment::GlobalsBuilder;

/// Functions defined in `buck2_bxl` crate,
/// which are used to create the context for `.bxl` evaluation.
pub struct BxlFunctions {
    pub register_cli_args_struct: fn(&mut GlobalsBuilder),
    pub register_bxl_function: fn(&mut GlobalsBuilder),
    pub register_artifact_function: fn(&mut GlobalsBuilder),
    pub register_label_function: fn(&mut GlobalsBuilder),
    pub register_target_function: fn(&mut GlobalsBuilder),
    pub register_instant_function: fn(&mut GlobalsBuilder),
    pub register_error_handling_function: fn(&mut GlobalsBuilder),
}

/// Default version used in the `buck2` binary.
pub static BXL_FUNCTIONS: LateBinding<BxlFunctions> = LateBinding::new("BXL_FUNCTIONS");
