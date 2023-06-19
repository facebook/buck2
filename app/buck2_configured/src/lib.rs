/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]

pub mod calculation;
pub mod configuration;
pub mod nodes;

pub fn init_late_bindings() {
    calculation::init_configured_target_calculation();
    nodes::calculation::init_configured_target_node_calculation();
}
