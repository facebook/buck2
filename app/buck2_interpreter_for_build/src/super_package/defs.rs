/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;

use crate::super_package::modifiers::register_get_parent_package_cfg_modifiers;
use crate::super_package::modifiers::register_set_package_cfg_modifiers;
use crate::super_package::package::register_package_function;
use crate::super_package::package_value::register_write_package_value;

/// Globals for `PACKAGE` files and `bzl` files included from `PACKAGE` files.
pub(crate) fn register_package_natives(globals: &mut GlobalsBuilder) {
    register_package_function(globals);
    register_write_package_value(globals);
    register_set_package_cfg_modifiers(globals);
    register_get_parent_package_cfg_modifiers(globals);
}
