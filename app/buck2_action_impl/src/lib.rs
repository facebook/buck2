/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(try_blocks)]
#![feature(type_alias_impl_trait)]
#![feature(impl_trait_in_assoc_type)]

use std::sync::Once;

mod actions;
mod context;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        actions::impls::run::audit_dep_files::init_audit_dep_files();
        actions::impls::run::dep_files::init_flush_dep_files();
        context::init_analysis_action_methods_actions();
    });
}
