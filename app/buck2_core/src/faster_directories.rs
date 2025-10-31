/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::atomic::AtomicBool;

pub static VALUE: AtomicBool = AtomicBool::new(true);

/// Is faster directories enabled?
///
/// NOTE: Can change with no warning.
pub fn is_enabled() -> bool {
    VALUE.load(std::sync::atomic::Ordering::Relaxed)
}
