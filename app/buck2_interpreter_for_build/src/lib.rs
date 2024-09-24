/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(try_blocks)]

use std::sync::Once;

pub mod attrs;
pub mod call_stack;
pub mod interpreter;
pub mod label;
pub mod nodes;
pub(crate) mod plugins;
pub mod rule;
pub mod super_package;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        attrs::attrs_global::init_coerce_target_label_for_bzl();
        interpreter::calculation::init_interpreter_calculation_impl();
        interpreter::calculation::init_target_graph_calculation_impl();
        interpreter::build_context::init_starlark_path_from_build_context();
        plugins::init_plugin_kind_from_value_impl();
        rule::init_frozen_rule_get_impl();
        rule::init_frozen_promise_artifact_mappings_get_impl();
    });
}
