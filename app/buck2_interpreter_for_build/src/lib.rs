/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(try_blocks)]

use std::sync::Once;

pub mod attrs;
pub mod interpreter;
pub mod label;
pub mod load_signals;
pub mod nodes;
pub mod provider;
pub mod rule;
pub mod super_package;
pub mod transition;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(interpreter::calculation::init_target_graph_calculation_impl);
}
