/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use allocative::Allocative;
use anyhow::Context;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::none::NoneType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark_map::small_map::SmallMap;

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

#[derive(
    Default,
    Debug,
    derive_more::Display,
    Allocative,
    ProvidesStaticType,
    Trace,
    NoSerialize
)]
#[display(fmt = "{:?}", self)]
pub(crate) struct PackageValues<'v> {
    pub(crate) values: RefCell<SmallMap<String, Value<'v>>>,
}

impl<'v> StarlarkValue<'v> for PackageValues<'v> {
    starlark_type!("PackageValues");
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

        let extra_value = eval
            .module()
            .extra_value()
            .context("Module extra value was not set (internal error)")?;
        let package_values = extra_value
            .downcast_ref::<PackageValues>()
            .context("Module extra value was not a `PackageValues` (internal error)")?;

        let mut package_values = package_values.values.borrow_mut();

        if package_values.contains_key(key) {
            return Err(PackageValueError::KeyAlreadySetInThisFile(key.to_owned()).into());
        }

        if !overwrite {
            if package_ctx.parent.package_values().contains_key(key) {
                return Err(PackageValueError::KeySetInParentFile(key.to_owned()).into());
            }
        }

        package_values.insert(key.to_owned(), value);

        Ok(NoneType)
    }
}

#[starlark_module]
pub fn register_read_package_value(globals: &mut GlobalsBuilder) {
    /// Read value specified in the `PACKAGE` file.
    ///
    /// Returns `None` if value is not set.
    #[starlark(return_type = "[None, str.type]")]
    fn read_package_value<'v>(
        #[starlark(require = pos)] key: &str,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        validate_key(key)?;

        let build_ctx = ModuleInternals::from_context(eval, "read_package_value")?;
        match build_ctx.super_package.package_values().get(key) {
            Some(value) => Ok(value.owned_value(eval.frozen_heap())),
            None => Ok(Value::new_none()),
        }
    }
}
