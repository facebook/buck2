/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! `ConstraintValueInfo` is the provider info returned from a `constraint_value()` rule.

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    values::{Freeze, Trace, UnpackValue, ValueLike, ValueOf, ValueTyped},
};

use crate::interpreter::rule_defs::{
    provider::constraint_setting_info::ConstraintSettingInfo, target_label::StarlarkTargetLabel,
};

/// Provider that signals that a target can be used as a constraint key. This is the only provider
/// returned by a `constraint_value()` target.
#[internal_provider(constraint_value_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub(crate) struct ConstraintValueInfoGen<V> {
    /// ConstraintSettingInfo
    setting: V,
    /// StarlarkTargetLabel
    label: V,
}

impl<'v, V: ValueLike<'v>> ConstraintValueInfoGen<V> {
    pub(crate) fn setting(&self) -> ValueOf<'v, &'v ConstraintSettingInfo<'v>> {
        ValueOf::unpack_value(self.setting.to_value()).expect("validated at construction")
    }

    pub(crate) fn label(&self) -> ValueTyped<'v, StarlarkTargetLabel> {
        ValueTyped::new(self.label.to_value()).expect("validated at construction")
    }
}

impl<'v> ConstraintValueInfo<'v> {
    pub(crate) fn new(
        setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        label: ValueOf<'v, &'v StarlarkTargetLabel>,
    ) -> ConstraintValueInfo<'v> {
        ConstraintValueInfoGen {
            setting: setting.value,
            label: label.value,
        }
    }
}

#[starlark_module]
fn constraint_value_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "ConstraintValueInfo")]
    fn ConstraintValueInfo<'v>(
        #[starlark(require = named)] setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        #[starlark(require = named)] label: ValueOf<'v, &'v StarlarkTargetLabel>,
    ) -> anyhow::Result<ConstraintValueInfo<'v>> {
        Ok(ConstraintValueInfo {
            setting: *setting,
            label: *label,
        })
    }
}
