/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::metadata_key::MetadataKey;
use buck2_core::metadata_key::MetadataKeyRef;
use buck2_interpreter::file_type::StarlarkFileType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::Value;

use crate::interpreter::build_context::BuildContext;

#[derive(Debug, thiserror::Error)]
enum PackageValueError {
    #[error("key already set in this file: `{0}`")]
    KeyAlreadySetInThisFile(MetadataKey),
    #[error("key set in parent `PACKAGE` file, and overwrite flag is not set: `{0}`")]
    KeySetInParentFile(MetadataKey),
}

#[starlark_module]
pub fn register_write_package_value(globals: &mut GlobalsBuilder) {
    /// Set the value to be accessible in the nested `PACKAGE` files.
    ///
    /// If any parent `PACKAGE` value has already set the same `key`,
    /// it will raise an error unless you pass `overwrite = True`,
    /// in which case it will replace the parent value.
    fn write_package_value<'v>(
        #[starlark(require = pos)] key: &str,
        #[starlark(require = pos)] value: Value<'v>,
        #[starlark(require = named, default = false)] overwrite: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<NoneType> {
        let key = MetadataKeyRef::new(key)?;

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
        let key = MetadataKeyRef::new(key)?;

        let build_ctx = BuildContext::from_context(eval)?;
        let build_ctx = build_ctx
            .additional
            .require_build("read_package_value")
            .map_err(|err| {
                let file_type = build_ctx.additional.file_type();

                if file_type == StarlarkFileType::Package {
                    err.context(format!(
                        "In a {file_type:?} context, consider using `read_parent_package_value`"
                    ))
                } else {
                    err
                }
            })?;

        match build_ctx.super_package.package_values().get(key) {
            Some(value) => Ok(eval.heap().alloc(value)),
            None => Ok(Value::new_none()),
        }
    }

    /// Read a package value defined in a parent `PACKAGE` file.
    ///
    /// This function can only be called in a Package context.
    ///
    /// Returns `None` if value is not set.
    fn read_parent_package_value<'v>(
        #[starlark(require = pos)] key: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let key = MetadataKeyRef::new(key)?;

        let package_ctx = BuildContext::from_context(eval)?
            .additional
            .require_package_file("read_parent_package_value")?;

        match package_ctx.parent.package_values().get(key) {
            Some(value) => Ok(eval.heap().alloc(value)),
            None => Ok(Value::new_none()),
        }
    }
}
