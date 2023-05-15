/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(trait_alias)]
#![feature(try_blocks)]
#![feature(provide_any)]

use std::sync::Once;

pub mod bxl;
pub mod command;
pub mod profile_command;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        bxl::starlark_defs::init_configure_bxl_file_globals();
        bxl::calculation::init_bxl_calculation_impl();
    });
}

#[test]
fn init_late_bindings_for_test() {
    #[ctor::ctor]
    fn init() {
        init_late_bindings();
    }
}
