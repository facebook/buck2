/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(box_patterns)]
#![feature(iter_order_by)]
#![feature(try_blocks)]
#![feature(once_cell_try)]
#![feature(used_with_arg)]
#![feature(iterator_try_collect)]

#[macro_use]
extern crate starlark;

use std::sync::Once;

pub mod actions;
pub mod analysis;
pub mod anon_target;
pub mod artifact_groups;
pub mod attrs;
pub mod audit_cell;
pub mod audit_dep_files;
pub mod audit_output;
pub mod build;
pub mod build_signals;
pub mod bxl;
pub mod configure_dice;
pub mod configure_targets;
pub mod context;
pub mod deferred;
pub mod dynamic;
pub mod dynamic_value;
pub mod interpreter;
pub mod keep_going;
pub mod materialize;
pub mod query;
pub mod spawner;
pub mod transition;
pub mod validation;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        interpreter::more::init_register_build_api_globals();
        interpreter::rule_defs::context::init_analysis_context_ty();
        interpreter::rule_defs::provider::ty::abstract_provider::init_provider_ty();
    });
}

#[doc(hidden)]
pub mod __derive_refs {
    pub use display_container;
    pub use inventory;
    pub use serde;
}
