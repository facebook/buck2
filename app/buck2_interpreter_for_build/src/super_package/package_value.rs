/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::io;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_node::metadata::key::MetadataKey;
use buck2_node::metadata::key::MetadataKeyRef;
use buck2_node::metadata::super_package_values::SuperPackageValues;
use dupe::Dupe;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::FrozenHeapRef;
use starlark::values::FrozenValue;
use starlark::values::OwnedFrozenValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::interpreter::build_context::BuildContext;
use crate::interpreter::package_file_extra::PackageFileExtra;

#[derive(Debug, thiserror::Error)]
enum PackageValueError {
    #[error("key already set in this file: `{0}`")]
    KeyAlreadySetInThisFile(MetadataKey),
    #[error("key set in parent `PACKAGE` file, and overwrite flag is not set: `{0}`")]
    KeySetInParentFile(MetadataKey),
}

#[derive(Debug, Default, Allocative)]
pub struct SuperPackageValuesImpl {
    pub(crate) values: SmallMap<MetadataKey, OwnedFrozenStarlarkPackageValue>,
}

impl SuperPackageValuesImpl {
    pub(crate) fn get(values: &dyn SuperPackageValues) -> anyhow::Result<&SuperPackageValuesImpl> {
        values
            .as_any()
            .downcast_ref::<SuperPackageValuesImpl>()
            .context("Expecting SuperPackageValuesImpl (internal error)")
    }

    pub(crate) fn merge(
        parent: &Arc<dyn SuperPackageValues>,
        this_package: SmallMap<MetadataKey, OwnedFrozenStarlarkPackageValue>,
    ) -> anyhow::Result<Arc<dyn SuperPackageValues>> {
        if this_package.is_empty() {
            Ok(parent.dupe())
        } else {
            let parent = Self::get(&**parent)?;
            let mut merged_package_values = parent.values.clone();
            merged_package_values.extend(this_package);
            Ok(Arc::new(SuperPackageValuesImpl {
                values: merged_package_values,
            }))
        }
    }
}

impl SuperPackageValues for SuperPackageValuesImpl {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    fn package_values_json(&self) -> anyhow::Result<SmallMap<MetadataKey, serde_json::Value>> {
        let mut values = SmallMap::with_capacity(self.values.len());
        for (key, value) in &self.values {
            values.insert(key.clone(), value.to_json_value()?);
        }
        Ok(values)
    }

    fn contains_key(&self, key: &MetadataKeyRef) -> bool {
        self.values.contains_key(key)
    }
}

/// Value that is known to be serializable to JSON.
#[derive(Trace, Debug, Allocative, Clone, Dupe, Copy)]
pub(crate) struct StarlarkPackageValue<'v>(Value<'v>);

#[derive(Debug, Allocative, Clone, Dupe, Copy)]
pub(crate) struct FrozenStarlarkPackageValue(FrozenValue);

#[derive(Debug, Allocative, Clone, Dupe)]
pub(crate) struct OwnedFrozenStarlarkPackageValue(OwnedFrozenValue);

impl<'v> StarlarkPackageValue<'v> {
    pub(crate) fn new(value: Value<'v>) -> anyhow::Result<StarlarkPackageValue<'v>> {
        serde_json::to_writer(io::sink(), &value)
            .context("Value must be serializable to JSON to be stored as package value")?;
        Ok(StarlarkPackageValue(value))
    }
}

impl<'v> Freeze for StarlarkPackageValue<'v> {
    type Frozen = FrozenStarlarkPackageValue;

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let frozen = self.0.freeze(freezer)?;

        // Error is possible if either:
        // * package value is modified after `write_package_value`
        // * frozen value is not valid JSON even if original value was
        StarlarkPackageValue::new(frozen.to_value()).context("Frozen value is not valid JSON")?;

        Ok(FrozenStarlarkPackageValue(frozen))
    }
}

impl OwnedFrozenStarlarkPackageValue {
    /// This function is unsafe for the same reason `OwnedFrozenValue::new` is unsafe:
    /// `owner` must be the owner of `value`.
    pub(crate) unsafe fn new(
        owner: FrozenHeapRef,
        value: FrozenStarlarkPackageValue,
    ) -> OwnedFrozenStarlarkPackageValue {
        OwnedFrozenStarlarkPackageValue(OwnedFrozenValue::new(owner, value.0))
    }

    pub(crate) fn to_json_value(&self) -> anyhow::Result<serde_json::Value> {
        self.0
            .value()
            .to_json_value()
            .context("Not valid JSON, should have been validated at construction (internal error)")
    }

    pub(crate) fn owned_frozen_value(&self) -> &OwnedFrozenValue {
        &self.0
    }
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

        let package_file_extra = PackageFileExtra::get_or_init(eval)?;

        if package_file_extra.package_values.borrow().contains_key(key) {
            return Err(PackageValueError::KeyAlreadySetInThisFile(key.to_owned()).into());
        }

        if !overwrite {
            if package_ctx.parent.package_values().contains_key(key) {
                return Err(PackageValueError::KeySetInParentFile(key.to_owned()).into());
            }
        }

        let value = StarlarkPackageValue::new(value)?;

        package_file_extra
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

        match SuperPackageValuesImpl::get(&**build_ctx.super_package.package_values())?
            .values
            .get(key)
        {
            Some(value) => Ok(value.owned_frozen_value().owned_value(eval.frozen_heap())),
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

        match SuperPackageValuesImpl::get(&**package_ctx.parent.package_values())?
            .values
            .get(key)
        {
            Some(value) => Ok(value.owned_frozen_value().owned_value(eval.frozen_heap())),
            None => Ok(Value::new_none()),
        }
    }
}
