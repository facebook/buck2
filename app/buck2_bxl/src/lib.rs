/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(trait_alias)]
#![feature(try_blocks)]

use std::sync::Once;

pub(crate) mod bxl;
pub(crate) mod command;
mod commands;
pub(crate) mod profile_command;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        bxl::starlark_defs::globals::init_bxl_specific_globals();
        bxl::starlark_defs::context::dynamic::init_eval_bxl_for_dynamic_output();
        bxl::calculation::init_bxl_calculation_impl();
        commands::init_bxl_server_commands();
        bxl::starlark_defs::context::anon_target::init_eval_bxl_for_anon_target();
    });
}

#[test]
fn init_late_bindings_for_test() {
    #[ctor::ctor]
    fn init() {
        init_late_bindings();
    }
}
