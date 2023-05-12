/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::load_symbols::register_load_symbols;
use buck2_interpreter::functions::regex::register_regex;
use buck2_interpreter::functions::soft_error::register_soft_error;
use buck2_interpreter::functions::warning::register_warning;
use buck2_interpreter_for_build::attrs::attrs_global::register_attrs;
use buck2_interpreter_for_build::rule::register_rule_function;
use starlark::environment::GlobalsBuilder;

use crate::interpreter::rule_defs::provider::registration::register_builtin_providers;

pub mod artifact;
pub mod artifact_tagging;
pub mod cmd_args;
pub mod command_executor_config;
pub mod context;
pub mod label_relative_path;
pub mod provider;
pub mod transition;
pub mod transitive_set;

pub fn register_rule_defs(globals: &mut GlobalsBuilder) {
    register_attrs(globals);
    register_rule_function(globals);
    cmd_args::register_cmd_args(globals);
    register_builtin_providers(globals);
    register_load_symbols(globals);
    register_regex(globals);
    register_warning(globals);
    register_soft_error(globals);
}
