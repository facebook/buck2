/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;

use crate::interpreter::rule_defs::provider::registration::register_builtin_providers;

pub mod artifact;
pub mod artifact_tagging;
pub mod cmd_args;
pub mod command_executor_config;
pub mod context;
pub mod digest_config;
pub mod label_relative_path;
pub mod plugins;
pub mod provider;
pub mod resolve_query_macro;
pub mod resolved_macro;
pub mod transitive_set;

pub fn register_rule_defs(globals: &mut GlobalsBuilder) {
    cmd_args::register_cmd_args(globals);
    register_builtin_providers(globals);
}
