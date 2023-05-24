/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::more::MoreFunctions;
use buck2_interpreter::functions::more::MORE_FUNCTIONS;

use crate::attrs::attrs_global::register_attrs;
use crate::interpreter::functions::host_info::register_host_info;
use crate::interpreter::functions::read_config::register_read_config;
use crate::interpreter::natives::register_module_natives;
use crate::rule::register_rule_function;
use crate::super_package::defs::register_package_natives;
use crate::super_package::package_value::register_read_package_value;

pub(crate) fn init_more_functions() {
    MORE_FUNCTIONS.init(MoreFunctions {
        register_package_natives,
        register_attrs,
        register_rule_function,
        register_host_info,
        register_read_config,
        register_module_natives,
        register_read_package_value,
    });
}
