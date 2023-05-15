/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(try_blocks)]

use std::sync::Once;

pub mod analysis;
pub mod aquery;
pub mod cquery;
pub mod dice;
pub mod frontend;
pub mod uquery;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        analysis::environment::init_query_functions();
        analysis::eval::init_eval_analysis_query();
        frontend::init_query_frontend();
    });
}
