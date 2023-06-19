/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Debug;

use allocative::Allocative;
use buck2_build_api_derive::internal_provider;
use buck2_common::legacy_configs::parse_config_section_and_key;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::constraints::ConstraintKey;
use buck2_core::configuration::constraints::ConstraintValue;
use buck2_core::configuration::data::ConfigurationDataData;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::collections::SmallMap;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::values::dict::AllocDict;
use starlark::values::dict::Dict;
use starlark::values::dict::DictOf;
use starlark::values::dict::DictRef;
use starlark::values::type_repr::DictType;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::ValueOf;
use thiserror::Error;

use crate::interpreter::rule_defs::provider::builtin::constraint_setting_info::ConstraintSettingInfo;
use crate::interpreter::rule_defs::provider::builtin::constraint_value_info::ConstraintValueInfo;

/// Provider that signals that a rule contains configuration info. This is used both as part of
/// defining configurations (`platform()`, `constraint_value()`) and defining whether a target "matches"
/// a configuration or not (`config_setting()`, `constraint_value()`)
#[internal_provider(configuration_info_creator)]
#[derive(Debug, Trace, Coerce, Freeze, ProvidesStaticType, Allocative)]
#[repr(C)]
pub struct ConfigurationInfoGen<V> {
    #[provider(field_type = "DictType<StarlarkTargetLabel, ConstraintValueInfo>")]
    constraints: V,
    #[provider(field_type = "DictType<String, String>")]
    values: V,
}

impl<'v, V: ValueLike<'v>> ConfigurationInfoGen<V> {
    pub fn to_config_setting_data(&self) -> ConfigSettingData {
        let constraints =
            DictRef::from_value(self.constraints.to_value()).expect("type checked on construction");
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
            DictRef::from_value(self.values.to_value()).expect("type checked on construction");
        let mut converted_values = BTreeMap::new();
        for (k, v) in values.iter() {
            let key_config = k.to_value().to_str();
            let value_config = v.to_value().to_str();
            converted_values.insert(key_config, value_config);
        }

        ConfigSettingData {
            constraints: converted_constraints,
            buckconfigs: converted_values,
        }
    }

    pub fn to_configuration_data(&self) -> anyhow::Result<ConfigurationDataData> {
        let ConfigSettingData {
            constraints,
            buckconfigs,
        } = self.to_config_setting_data();
        if !buckconfigs.is_empty() {
            return Err(ConfigurationInfoError::BuckConfigsNotAllowed.into());
        }
        Ok(ConfigurationDataData { constraints })
    }
}

impl<'v> ConfigurationInfo<'v> {
    /// Create a provider from configuration data.
    pub fn from_configuration_data(conf: &ConfigurationDataData, heap: &'v Heap) -> Self {
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

        ConfigurationInfoGen {
            constraints: heap.alloc(Dict::new(constraints)),
            values: heap.alloc(AllocDict::EMPTY),
        }
    }
}

#[derive(Debug, Error)]
enum ConfigurationInfoError {
    #[error("key `{0}` in constraints dict does not match constraint value `{1}`")]
    ConstraintsKeyValueMismatch(String, String),
    #[error("`ConfigurationInfo` cannot have buckconfigs when it is used to create a platform")]
    BuckConfigsNotAllowed,
}

#[starlark_module]
fn configuration_info_creator(globals: &mut GlobalsBuilder) {
    #[starlark(dot_type = "ConfigurationInfo")]
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
