/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;

use crate::super_package::package::register_package_function;

/// Globals for `PACKAGE` files and `bzl` files included from `PACKAGE` files.
pub fn register_package_natives(globals: &mut GlobalsBuilder) {
    register_package_function(globals);
}
