/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![cfg(test)]

#[cfg(test)]
mod attr;
mod attrs;
mod functions;
pub mod interpreter;
mod label;
mod rule;
pub mod select;
mod super_package;
mod tests;
mod uncategorized;

#[test]
fn init_late_bindings_for_test() {
    #[ctor::ctor]
    fn init() {
        buck2_interpreter_for_build::init_late_bindings();
        buck2_build_api::init_late_bindings();
        buck2_transition::init_late_bindings();
    }
}
