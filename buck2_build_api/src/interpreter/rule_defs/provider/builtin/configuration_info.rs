/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::BTreeMap, fmt::Debug};

use buck2_build_api_derive::internal_provider;
use buck2_common::legacy_configs::parse_config_section_and_key;
use buck2_core::configuration::{
    constraints::{ConstraintKey, ConstraintValue},
    ConfigurationData,
};
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use gazebo::{
    any::ProvidesStaticType,
    coerce::{coerce, Coerce},
    prelude::*,
};
use starlark::{
    collections::SmallMap,
    environment::GlobalsBuilder,
    eval::Evaluator,
    values::{
        dict::{Dict, DictOf},
        Freeze, Heap, Trace, ValueLike, ValueOf,
    },
};
use thiserror::Error;

use crate::interpreter::rule_defs::provider::builtin::{
    constraint_setting_info::ConstraintSettingInfo, constraint_value_info::ConstraintValueInfo,
};

/// Provider that signals that a rule contains configuration info. This is used both as part of
/// defining configurations (`platform()`, `constraint_value()`) and defining whether a target "matches"
/// a configuration or not (`config_setting()`, `constraint_value()`)
#[internal_provider(configuration_info_creator)]
#[derive(Debug, Trace, Coerce, Freeze, ProvidesStaticType)]
#[repr(C)]
pub(crate) struct ConfigurationInfoGen<V> {
    // Dict<StarlarkTargetLabel, ConstraintValueInfo>
    constraints: V,
    // Dict<String, String>
    values: V,
}

impl<'v, V: ValueLike<'v>> ConfigurationInfoGen<V> {
    pub fn to_configuration_data(&self) -> ConfigurationData {
        let constraints =
            Dict::from_value(self.constraints.to_value()).expect("type checked on construction");
        let mut converted_constraints = BTreeMap::new();
        for (k, v) in constraints.iter() {
            let key_target = StarlarkTargetLabel::from_value(k.to_value())
                .expect("type checked on construction");
            let value_target = ConstraintValueInfo::from_value(v.to_value())
                .expect("type checked on construction");
            converted_constraints.insert(
                ConstraintKey(key_target.label().dupe()),
                ConstraintValue(value_target.label().label().dupe()),
            );
        }

        let values =
            Dict::from_value(self.values.to_value()).expect("type checked on construction");
        let mut converted_values = BTreeMap::new();
        for (k, v) in values.iter() {
            let key_config = k.to_value().to_str();
            let value_config = v.to_value().to_str();
            converted_values.insert(key_config, value_config);
        }

        ConfigurationData::new(converted_constraints, converted_values)
    }
}

impl<'v> ConfigurationInfo<'v> {
    /// Create a provider from configuration data.
    pub fn from_configuration_data(conf: &ConfigurationData, heap: &'v Heap) -> Self {
        let mut constraints = SmallMap::new();
        for (k, v) in &conf.constraints {
            let constraint_setting_label =
                heap.alloc_value_of(StarlarkTargetLabel::new(k.0.dupe()));
            let constraint_value_label = heap.alloc_value_of(StarlarkTargetLabel::new(v.0.dupe()));
            let constraint_setting =
                heap.alloc_value_of(ConstraintSettingInfo::new(constraint_setting_label));
            let constraint_value =
                ConstraintValueInfo::new(constraint_setting, constraint_value_label);
            let prev = constraints.insert_hashed(
                constraint_setting_label
                    .get_hashed()
                    .expect("StarlarkTargetLabel is hashable"),
                heap.alloc_complex(constraint_value),
            );
            assert!(prev.is_none());
        }

        let mut values = SmallMap::new();
        for (k, v) in &conf.buckconfigs {
            let prev = values.insert_hashed(heap.alloc_str(k).get_hashed(), heap.alloc_str(v));
            assert!(prev.is_none());
        }
        ConfigurationInfoGen {
            constraints: heap.alloc(Dict::new(constraints)),
            values: heap.alloc(Dict::new(coerce(values))),
        }
    }
}

#[derive(Debug, Error)]
enum ConfigurationInfoError {
    #[error("key `{0}` in constraints dict does not match constraint value `{1}`")]
    ConstraintsKeyValueMismatch(String, String),
}

#[starlark_module]
fn configuration_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(type = "ConfigurationInfo")]
    fn ConfigurationInfo<'v>(
        #[starlark(require = named)] constraints: DictOf<
            'v,
            ValueOf<'v, &'v StarlarkTargetLabel>,
            ValueOf<'v, &'v ConstraintValueInfo<'v>>,
        >,
        #[starlark(require = named)] values: DictOf<'v, &'v str, &'v str>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<ConfigurationInfo<'v>> {
        let mut new_constraints = SmallMap::new();
        for (constraint_setting, constraint_value) in constraints.collect_entries() {
            let constraint_setting_hashed = constraint_setting
                .value
                .get_hashed()
                .expect("should be hashable, we picked it from dict");
            let constraint_setting_from_constraint_value =
                constraint_value.typed.setting().typed.label();
            if *constraint_setting.typed != *constraint_setting_from_constraint_value {
                return Err(ConfigurationInfoError::ConstraintsKeyValueMismatch(
                    constraint_setting.value.to_string(),
                    constraint_value.to_string(),
                )
                .into());
            }
            let prev =
                new_constraints.insert_hashed(constraint_setting_hashed, constraint_value.value);
            assert!(prev.is_none());
        }
        for (k, _) in values.collect_entries() {
            // Validate the config section and key can be parsed correctly
            parse_config_section_and_key(k, None)?;
        }
        Ok(ConfigurationInfo {
            constraints: eval.heap().alloc(Dict::new(new_constraints)),
            values: *values,
        })
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::result::SharedResult;
    use indoc::indoc;

    use crate::interpreter::testing::{
        run_starlark_bzl_test, run_starlark_bzl_test_expecting_error,
    };

    #[test]
    fn configuration_info_validates_buckconfigs() -> SharedResult<()> {
        run_starlark_bzl_test_expecting_error(
            indoc!(
                r#"
            def test():
                ConfigurationInfo(
                    constraints = {},
                    values = {
                        "applekey": "value",
                    }
                )
            "#
            ),
            "Could not find section separator (`.`) in pair `applekey`",
        );

        run_starlark_bzl_test(indoc!(
            r#"
            def test():
                ConfigurationInfo(
                    constraints = {},
                    values = {
                        "apple.key": "value",
                    }
                )
            "#
        ))
    }
}
