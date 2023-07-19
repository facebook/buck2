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

use crate::interpreter::build_defs::register_provider;
use crate::interpreter::build_defs::register_transitive_set;
use crate::interpreter::rule_defs::cmd_args::register_cmd_args;
use crate::interpreter::rule_defs::command_executor_config::register_command_executor_config;
use crate::interpreter::rule_defs::context::register_analysis_context;
use crate::interpreter::rule_defs::provider::dependency::register_dependency;
use crate::interpreter::rule_defs::provider::registration::register_builtin_providers;
use crate::interpreter::rule_defs::register_rule_defs;

pub(crate) fn init_more_functions() {
    MORE_FUNCTIONS.init(MoreFunctions {
        register_builtin_providers,
        register_cmd_args,
        register_command_executor_config,
        register_provider,
        register_rule_defs,
        register_transitive_set,
        register_analysis_context,
        register_dependency,
    });
}
