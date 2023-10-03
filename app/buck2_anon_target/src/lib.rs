/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(box_patterns)]

use std::sync::Once;

pub(crate) mod anon_promises;
pub(crate) mod anon_target_attr;
pub(crate) mod anon_target_attr_coerce;
pub(crate) mod anon_target_attr_resolve;
pub(crate) mod anon_target_node;
pub(crate) mod anon_targets;
pub(crate) mod starlark_defs;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        anon_targets::init_anon_target_registry_new();
        anon_targets::init_eval_anon_target();
        starlark_defs::init_analysis_actions_methods_anon_target();
        starlark_defs::init_register_anon_target_types();
    });
}
