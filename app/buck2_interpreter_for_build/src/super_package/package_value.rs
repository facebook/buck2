/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::Value;

use crate::interpreter::build_context::BuildContext;
use crate::interpreter::module_internals::ModuleInternals;

#[derive(Debug, thiserror::Error)]
enum PackageValueError {
    #[error("key already set in this file: `{0}`")]
    KeyAlreadySetInThisFile(String),
    #[error("key set in parent `PACKAGE` file, and overwrite flag is not set: `{0}`")]
    KeySetInParentFile(String),
    #[error("key must contain exactly one dot: `{0}`")]
    KeyMustContainExactlyOneDot(String),
}

fn validate_key(key: &str) -> anyhow::Result<()> {
    if key.chars().filter(|c| *c == '.').count() != 1 {
        return Err(PackageValueError::KeyMustContainExactlyOneDot(key.to_owned()).into());
    }
    Ok(())
}

#[starlark_module]
pub(crate) fn register_write_package_value(globals: &mut GlobalsBuilder) {
    /// Set the value to be accessible in the nested `PACKAGE` files.
    fn write_package_value<'v>(
        #[starlark(require = pos)] key: &str,
        #[starlark(require = pos)] value: Value<'v>,
        #[starlark(require = named, default = false)] overwrite: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        validate_key(key)?;

        let package_ctx = BuildContext::from_context(eval)?
            .additional
            .require_package_file("write_package_value")?;

        if package_ctx.package_values.borrow().contains_key(key) {
            return Err(PackageValueError::KeyAlreadySetInThisFile(key.to_owned()).into());
        }

        if !overwrite {
            if package_ctx.parent.package_values().contains_key(key) {
                return Err(PackageValueError::KeySetInParentFile(key.to_owned()).into());
            }
        }

        let value = value
            .to_json_value()
            .context("Package values must be convertible to JSON")?;

        package_ctx
            .package_values
            .borrow_mut()
            .insert(key.to_owned(), value);

        Ok(NoneType)
    }
}

#[starlark_module]
pub fn register_read_package_value(globals: &mut GlobalsBuilder) {
    /// Read value specified in the `PACKAGE` file.
    ///
    /// Returns `None` if value is not set.
    fn read_package_value<'v>(
        #[starlark(require = pos)] key: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        validate_key(key)?;

        let build_ctx = ModuleInternals::from_context(eval, "read_package_value")?;
        match build_ctx.super_package.package_values().get(key) {
            Some(value) => Ok(eval.heap().alloc(value)),
            None => Ok(Value::new_none()),
        }
    }
}
