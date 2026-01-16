/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! `ConstraintValueInfo` is the provider info returned from a `constraint_value()` rule.

use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::LabelArg;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueOfUncheckedGeneric;
use starlark::values::ValueTyped;

use crate as buck2_build_api;
use crate::interpreter::rule_defs::provider::builtin::constraint_setting_info::ConstraintSettingInfo;
use crate::interpreter::rule_defs::provider::builtin::constraint_setting_info::FrozenConstraintSettingInfo;

/// Provider that signals that a target can be used as a constraint key. This is the only provider
/// returned by a `constraint_value()` target.
#[internal_provider(constraint_value_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct ConstraintValueInfoGen<V: ValueLifetimeless> {
    setting: ValueOfUncheckedGeneric<V, FrozenConstraintSettingInfo>,
    label: ValueOfUncheckedGeneric<V, StarlarkProvidersLabel>,
}

impl<'v, V: ValueLike<'v>> ConstraintValueInfoGen<V> {
    pub(crate) fn setting(&self) -> ValueOf<'v, &'v ConstraintSettingInfo<'v>> {
        ValueOf::unpack_value_err(self.setting.get().to_value()).expect("validated at construction")
    }

    pub(crate) fn label(&self) -> ValueTyped<'v, StarlarkProvidersLabel> {
        ValueTyped::new_err(self.label.get().to_value()).expect("validated at construction")
    }

    /// Convert to a ConstraintValue for use in configuration data.
    fn to_constraint_value(&self) -> ConstraintValue {
        ConstraintValue(self.label().label().dupe())
    }

    /// Get the ConstraintKey and ConstraintValue pair for use in configuration data.
    pub fn to_constraint_key_value(&self) -> (ConstraintKey, ConstraintValue) {
        let constraint_key = self.setting().typed.to_constraint_key();
        let constraint_value = self.to_constraint_value();
        (constraint_key, constraint_value)
    }
}

impl<'v> ConstraintValueInfo<'v> {
    pub(crate) fn new(
        setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        label: ValueOf<'v, &'v StarlarkProvidersLabel>,
    ) -> ConstraintValueInfo<'v> {
        ConstraintValueInfoGen {
            setting: ValueOfUnchecked::new(setting.value),
            label: label.as_unchecked().cast(),
        }
    }

    pub(crate) fn default_from_constraint_setting(
        setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
    ) -> Option<ConstraintValueInfo<'v>> {
        match setting.typed.default() {
            Some(default_provider_label) => {
                let constraint_value = ConstraintValueInfo::new(
                    setting,
                    ValueOf::unpack_value_err(default_provider_label.to_value())
                        .expect("validated at construction"),
                );
                Some(constraint_value)
            }
            None => None,
        }
    }
}

#[starlark_module]
fn constraint_value_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenConstraintValueInfo)]
    fn ConstraintValueInfo<'v>(
        #[starlark(require = named)] setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        #[starlark(require = named)] label: LabelArg<'v>,
        heap: Heap<'v>,
    ) -> starlark::Result<ConstraintValueInfo<'v>> {
        let provider_label = label.to_provider_label();
        let label = heap.alloc_value_of(provider_label);
        Ok(ConstraintValueInfo::new(setting, label))
    }
}
