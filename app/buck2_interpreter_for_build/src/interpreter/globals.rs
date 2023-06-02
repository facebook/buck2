/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::bxl::BXL_FUNCTIONS;
use buck2_interpreter::functions::more::MORE_FUNCTIONS;
use buck2_interpreter::functions::transition::REGISTER_TRANSITION;
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

/// Natives for `BUCK` and `bzl` files.
fn register_build_bzl_natives(builder: &mut GlobalsBuilder) {
    (MORE_FUNCTIONS.get().unwrap().register_provider)(builder);
    (MORE_FUNCTIONS.get().unwrap().register_transitive_set)(builder);
    register_module_natives(builder);
    register_host_info(builder);
    register_read_config(builder);
    register_read_package_value(builder);
}

/// Globals for `BUCK` files.
pub fn configure_build_file_globals(globals_builder: &mut GlobalsBuilder) {
    // TODO(cjhopman): This unconditionally adds the native symbols to the global
    // env, but that needs to be a cell-based config.
    register_build_bzl_natives(globals_builder);
}

/// Globals for `PACKAGE` files.
pub fn configure_package_file_globals(globals_builder: &mut GlobalsBuilder) {
    // TODO(cjhopman): This unconditionally adds the native symbols to the global
    // env, but that needs to be a cell-based config.
    register_build_bzl_natives(globals_builder);
    register_package_natives(globals_builder);
}

/// Globals for `.bxl` files.
pub fn configure_bxl_file_globals(globals_builder: &mut GlobalsBuilder) {
    register_base_natives(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_cmd_args)(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_cli_args_struct)(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_bxl_function)(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_artifact_function)(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_label_function)(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_target_function)(globals_builder);
    register_read_config(globals_builder);
    register_host_info(globals_builder);
    (BXL_FUNCTIONS.get().unwrap().register_instant_function)(globals_builder);
    (BXL_FUNCTIONS
        .get()
        .unwrap()
        .register_error_handling_function)(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_builtin_providers)(globals_builder);
}

/// Globals for `.bzl` files.
pub fn configure_extension_file_globals(globals_builder: &mut GlobalsBuilder) {
    // TODO(cjhopman): This unconditionally adds the native symbols to the global
    // env, but that needs to be a cell-based config.
    register_build_bzl_natives(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_cmd_args)(globals_builder);
    (MORE_FUNCTIONS.get().unwrap().register_rule_defs)(globals_builder);
    register_warning(globals_builder);
    register_soft_error(globals_builder);
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
}
