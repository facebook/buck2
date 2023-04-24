/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::Value;

use crate::interpreter::module_internals::ModuleInternals;

#[starlark_module]
pub fn register_module_natives(globals: &mut GlobalsBuilder) {
    /// Check if the target with `name` has already been defined,
    /// returns `True` if it has.
    ///
    /// Note that this function checks for the existence of a _target_ rather than a _rule_.
    /// In general use of this function is discouraged, as it makes definitions of rules not compose.
    fn rule_exists(name: &str, eval: &mut Evaluator) -> anyhow::Result<bool> {
        Ok(ModuleInternals::from_context(eval, "rule_exists")?.target_exists(name))
    }

    /// Called in a `BUCK` file to declare the oncall contact details for
    /// all the targets defined. Must be called at most once, before any targets
    /// have been declared. Errors if called from a `.bzl` file.
    fn oncall(
        #[starlark(require = pos)] name: &str,
        eval: &mut Evaluator,
    ) -> anyhow::Result<NoneType> {
        let internals = ModuleInternals::from_context(eval, "oncall")?;
        internals.set_oncall(name)?;
        Ok(NoneType)
    }

    fn implicit_package_symbol<'v>(
        name: &str,
        default: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let internals = ModuleInternals::from_context(eval, "implicit_package_symbol")?;
        match internals.get_package_implicit(name) {
            None => Ok(default.unwrap_or_else(Value::new_none)),
            Some(v) => {
                // FIXME(ndmitchell): Document why this is safe
                Ok(unsafe { v.unchecked_frozen_value().to_value() })
            }
        }
    }
}
