/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::bxl::BXL_SPECIFIC_GLOBALS;
use buck2_interpreter::functions::more::MORE_FUNCTIONS;
use buck2_interpreter::functions::transition::REGISTER_TRANSITION;
use buck2_interpreter::types::configured_providers_label::register_configured_providers_label;
use buck2_interpreter::types::target_label::register_target_label;
use starlark::environment::GlobalsBuilder;

use crate::attrs::attrs_global::register_attrs;
use crate::interpreter::build_defs::register_base_natives;
use crate::interpreter::functions::host_info::register_host_info;
use crate::interpreter::functions::load_symbols::register_load_symbols;
use crate::interpreter::functions::read_config::register_read_config;
use crate::interpreter::functions::regex::register_regex;
use crate::interpreter::functions::soft_error::register_soft_error;
use crate::interpreter::functions::warning::register_warning;
use crate::interpreter::natives::register_module_natives;
use crate::rule::register_rule_function;
use crate::super_package::defs::register_package_natives;
use crate::super_package::package_value::register_read_package_value;

// TODO(nga): Unify globals.
//   It was decided (https://fburl.com/workplace/dlvp5c9q)
//   that we want identical globals for all files, except `BUCK` files,
//   where we additionally add prelude and package implicits.

/// Natives for all file types.
fn register_universal_natives(builder: &mut GlobalsBuilder) {
    (MORE_FUNCTIONS.get().unwrap().register_provider)(builder);
    (MORE_FUNCTIONS.get().unwrap().register_transitive_set)(builder);
    register_module_natives(builder);
    register_host_info(builder);
    register_read_config(builder);
    register_read_package_value(builder);
    register_soft_error(builder);
    register_package_natives(builder);
    (MORE_FUNCTIONS.get().unwrap().register_cmd_args)(builder);
}

/// Globals for `BUCK` files.
pub fn configure_build_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_universal_natives(globals_builder);
}

/// Globals for `PACKAGE` files.
pub fn configure_package_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_universal_natives(globals_builder);
}

/// Globals for `.bxl` files.
pub fn configure_bxl_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_universal_natives(globals_builder);
    register_base_natives(globals_builder);
    (BXL_SPECIFIC_GLOBALS.get().unwrap())(globals_builder);
    register_read_config(globals_builder);
    register_host_info(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_builtin_providers)(globals_builder);
}

/// Globals for `.bzl` files.
pub fn configure_extension_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_universal_natives(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_rule_defs)(globals_builder);
    register_warning(globals_builder);
    register_regex(globals_builder);
    register_load_symbols(globals_builder);
    register_rule_function(globals_builder);
    register_attrs(globals_builder);
    (REGISTER_TRANSITION.get().unwrap())(globals_builder);
    (MORE_FUNCTIONS
        .get()
        .unwrap()
        .register_command_executor_config)(globals_builder);
    register_package_natives(globals_builder);
    register_configured_providers_label(globals_builder);
    register_target_label(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_analysis_context)(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_dependency)(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_artifact)(globals_builder);
}
