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
#![feature(try_blocks)]

pub mod analysis;
pub mod attrs;

pub fn init_late_bindings() {
    analysis::calculation::init_rule_analysis_calculation();
}
