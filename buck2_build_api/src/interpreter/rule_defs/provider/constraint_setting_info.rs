/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! ConstraintSettingInfo is the provider info returned from a constraint_setting() rule. Currently, all
//! it really does is indicate that the rule is actually a constraint_setting as a constraint_setting()
//! rule has no arguments.
//!
//! A constraint_value() rule will check that the referenced setting produces a ConstraintSettingInfo.

use std::fmt::Debug;

use buck2_build_api_derive::internal_provider;
use gazebo::{any::ProvidesStaticType, coerce::Coerce};
use starlark::{
    environment::GlobalsBuilder,
    values::{Freeze, Trace, ValueLike, ValueOf, ValueTyped},
};

use crate::interpreter::rule_defs::target_label::StarlarkTargetLabel;

/// Provider that signals that a target can be used as a constraint key. This is the only provider
/// returned by a `constraint_setting()` target.
#[internal_provider(constraint_info_creator)]
#[derive(Clone, Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(transparent)]
pub(crate) struct ConstraintSettingInfoGen<V> {
    /// StarlarkTargetLabel
    label: V,
}

impl<'v, V: ValueLike<'v>> ConstraintSettingInfoGen<V> {
    pub(crate) fn label(&self) -> ValueTyped<'v, StarlarkTargetLabel> {
        ValueTyped::new(self.label.to_value()).expect("validated at construction")
    }
}

impl<'v> ConstraintSettingInfo<'v> {
    pub(crate) fn new(label: ValueOf<'v, &'v StarlarkTargetLabel>) -> ConstraintSettingInfo<'v> {
        ConstraintSettingInfoGen { label: label.value }
    }
}

#[starlark_module]
fn constraint_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "ConstraintSettingInfo")]
    fn ConstraintSettingInfo<'v>(
        label: ValueOf<'v, &'v StarlarkTargetLabel>,
    ) -> anyhow::Result<ConstraintSettingInfo<'v>> {
        Ok(ConstraintSettingInfo { label: *label })
    }
}
