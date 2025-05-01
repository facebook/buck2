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
#![feature(impl_trait_in_assoc_type)]
#![feature(used_with_arg)]

use std::sync::Once;

mod actions;
mod context;
pub mod dynamic;
mod starlark_defs;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        actions::impls::run::audit_dep_files::init_audit_dep_files();
        actions::impls::run::dep_files::init_flush_dep_files();
        context::init_analysis_action_methods_actions();
        dynamic::calculation::init_dynamic_lambda_calculation();
        dynamic::storage::init_dynamic_lambda_params_storages();
        starlark_defs::init_register_buck2_action_impl_globals();
    });
}
