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
#![feature(used_with_arg)]

pub(crate) mod coerced_attr;
mod transition;

pub fn init_late_bindings() {
    transition::calculation_apply_transition::init_transition_calculation();
}
