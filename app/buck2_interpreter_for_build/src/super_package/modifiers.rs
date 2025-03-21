/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_node::modifiers::MODIFIER_METADATA_KEY;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::Value;

use crate::super_package::package_value::read_parent_package_value_impl;
use crate::super_package::package_value::write_package_value_impl;

#[starlark_module]
pub(crate) fn register_set_package_cfg_modifiers(globals: &mut GlobalsBuilder) {
    /// Set configuration modifiers for `PACKAGE` files.
    ///
    /// This function can only be called in a Package context.
    fn set_modifiers<'v>(
        #[starlark(require = pos)] value: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<NoneType> {
        write_package_value_impl(MODIFIER_METADATA_KEY, value, true, eval, "set_modifiers")
    }
}

#[starlark_module]
pub(crate) fn register_get_parent_package_cfg_modifiers(globals: &mut GlobalsBuilder) {
    /// Get a configuration modifiers defined in a parent `PACKAGE` file.
    ///
    /// This function can only be called in a Package context.
    ///
    /// Returns `None` if value is not set.
    fn get_parent_modifiers<'v>(eval: &mut Evaluator<'v, '_, '_>) -> starlark::Result<Value<'v>> {
        read_parent_package_value_impl(MODIFIER_METADATA_KEY, eval, "get_parent_modifiers")
    }
}
