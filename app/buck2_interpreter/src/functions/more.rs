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

/// Functions defined in `buck2_build_api`.
pub struct MoreFunctions {
    pub register_builtin_providers: fn(&mut GlobalsBuilder),
    pub register_cmd_args: fn(&mut GlobalsBuilder),
    pub register_command_executor_config: fn(&mut GlobalsBuilder),
    pub register_provider: fn(&mut GlobalsBuilder),
    pub register_rule_defs: fn(&mut GlobalsBuilder),
    pub register_transitive_set: fn(&mut GlobalsBuilder),
    pub register_analysis_context: fn(&mut GlobalsBuilder),
}

pub static MORE_FUNCTIONS: LateBinding<MoreFunctions> = LateBinding::new("MORE_FUNCTIONS");
