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

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
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
pub(crate) struct ConstraintValueInfoGen<V: ValueLifetimeless> {
    setting: ValueOfUncheckedGeneric<V, FrozenConstraintSettingInfo>,
    label: ValueOfUncheckedGeneric<V, StarlarkTargetLabel>,
}

impl<'v, V: ValueLike<'v>> ConstraintValueInfoGen<V> {
    pub(crate) fn setting(&self) -> ValueOf<'v, &'v ConstraintSettingInfo<'v>> {
        ValueOf::unpack_value_err(self.setting.get().to_value()).expect("validated at construction")
    }

    pub(crate) fn label(&self) -> ValueTyped<'v, StarlarkTargetLabel> {
        ValueTyped::new_err(self.label.get().to_value()).expect("validated at construction")
    }
}

impl<'v> ConstraintValueInfo<'v> {
    pub(crate) fn new(
        setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        label: ValueOf<'v, &'v StarlarkTargetLabel>,
    ) -> ConstraintValueInfo<'v> {
        ConstraintValueInfoGen {
            setting: ValueOfUnchecked::new(setting.value),
            label: label.as_unchecked().cast(),
        }
    }
}

#[starlark_module]
fn constraint_value_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(as_type = FrozenConstraintValueInfo)]
    fn ConstraintValueInfo<'v>(
        #[starlark(require = named)] setting: ValueOf<'v, &'v ConstraintSettingInfo<'v>>,
        #[starlark(require = named)] label: ValueOf<'v, &'v StarlarkTargetLabel>,
    ) -> starlark::Result<ConstraintValueInfo<'v>> {
        Ok(ConstraintValueInfo {
            setting: ValueOfUnchecked::new(setting.value),
            label: label.as_unchecked().cast(),
        })
    }
}
