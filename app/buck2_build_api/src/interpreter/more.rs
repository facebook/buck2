/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_interpreter::functions::more::REGISTER_BUCK2_BUILD_API_GLOBALS;
use starlark::environment::GlobalsBuilder;

use crate::interpreter::rule_defs::artifact::artifact_type::register_artifact;
use crate::interpreter::rule_defs::cmd_args::register_cmd_args;
use crate::interpreter::rule_defs::command_executor_config::register_command_executor_config;
use crate::interpreter::rule_defs::context::register_analysis_context;
use crate::interpreter::rule_defs::provider::callable::register_provider;
use crate::interpreter::rule_defs::provider::dependency::register_dependency;
use crate::interpreter::rule_defs::provider::registration::register_builtin_providers;
use crate::interpreter::rule_defs::register_rule_defs;
use crate::interpreter::rule_defs::transitive_set::globals::register_transitive_set_types;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition::register_transitive_set;

fn register_build_api_globals(globals: &mut GlobalsBuilder) {
    register_builtin_providers(globals);
    register_cmd_args(globals);
    register_command_executor_config(globals);
    register_provider(globals);
    register_rule_defs(globals);
    register_transitive_set(globals);
    register_transitive_set_types(globals);
    register_analysis_context(globals);
    register_dependency(globals);
    register_artifact(globals);
}

pub(crate) fn init_register_build_api_globals() {
    REGISTER_BUCK2_BUILD_API_GLOBALS.init(register_build_api_globals);
}
