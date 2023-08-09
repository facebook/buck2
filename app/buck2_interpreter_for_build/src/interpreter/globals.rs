/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::bxl::BXL_SPECIFIC_GLOBALS;
use buck2_interpreter::functions::more::REGISTER_BUCK2_BUILD_API_GLOBALS;
use buck2_interpreter::functions::transition::REGISTER_TRANSITION;
use buck2_interpreter::types::configured_providers_label::register_providers_label;
use buck2_interpreter::types::target_label::register_target_label;
use starlark::environment::GlobalsBuilder;

use crate::attrs::attrs_global::register_attrs;
use crate::interpreter::build_defs::register_path;
use crate::interpreter::functions::dedupe::register_dedupe;
use crate::interpreter::functions::host_info::register_host_info;
use crate::interpreter::functions::load_symbols::register_load_symbols;
use crate::interpreter::functions::read_config::register_read_config;
use crate::interpreter::functions::regex::register_regex;
use crate::interpreter::functions::sha256::register_sha256;
use crate::interpreter::functions::soft_error::register_soft_error;
use crate::interpreter::functions::warning::register_warning;
use crate::interpreter::natives::register_module_natives;
use crate::interpreter::selector::register_select;
use crate::plugins::register_plugins;
use crate::rule::register_rule_function;
use crate::super_package::defs::register_package_natives;
use crate::super_package::package_value::register_read_package_value;

/// Natives for all file types.
/// [It was decided](https://fburl.com/workplace/dlvp5c9q)
/// that we want identical globals for all files, except `BUCK` files,
/// where we additionally add prelude and package implicits.
pub fn register_universal_natives(builder: &mut GlobalsBuilder) {
    (REGISTER_BUCK2_BUILD_API_GLOBALS.get().unwrap())(builder);
    (REGISTER_TRANSITION.get().unwrap())(builder);
    (BXL_SPECIFIC_GLOBALS.get().unwrap())(builder);
    register_module_natives(builder);
    register_host_info(builder);
    register_read_config(builder);
    register_read_package_value(builder);
    register_soft_error(builder);
    register_package_natives(builder);
    register_warning(builder);
    register_regex(builder);
    register_load_symbols(builder);
    register_rule_function(builder);
    register_attrs(builder);
    register_plugins(builder);
    register_providers_label(builder);
    register_target_label(builder);
    register_path(builder);
    register_select(builder);
    register_sha256(builder);
    register_dedupe(builder);
}
