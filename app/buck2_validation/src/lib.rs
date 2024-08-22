/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(error_generic_member_access)]
#![feature(assert_matches)]
#![feature(round_char_boundary)]

use std::sync::Once;

mod cached_validation_result;
pub mod enabled_optional_validations_key;
mod single_validation_key;
mod transitive_validation_key;
mod validation;
mod validator_api;

pub fn init_late_bindings() {
    static ONCE: Once = Once::new();
    ONCE.call_once(|| {
        validation::init_validation_impl();
    });
}
