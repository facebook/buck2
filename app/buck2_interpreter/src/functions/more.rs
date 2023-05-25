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

/// Functions defined in `buck2_interpreter_for_build`.
pub struct MoreFunctions {
    pub register_package_natives: fn(&mut GlobalsBuilder),
    pub register_attrs: fn(&mut GlobalsBuilder),
    pub register_rule_function: fn(&mut GlobalsBuilder),
    pub register_host_info: fn(&mut GlobalsBuilder),
    pub register_read_config: fn(&mut GlobalsBuilder),
    pub register_module_natives: fn(&mut GlobalsBuilder),
    pub register_read_package_value: fn(&mut GlobalsBuilder),
    pub register_base_natives: fn(&mut GlobalsBuilder),
}

pub static MORE_FUNCTIONS: LateBinding<MoreFunctions> = LateBinding::new("MORE_FUNCTIONS");
