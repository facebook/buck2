/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::collections::sorted_map::SortedMap;
use buck2_core::configuration::Configuration;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use buck2_data::ToProtoMessage;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::rule_type::StarlarkRuleType;
use derive_more::Display;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Display, Allocative)]
#[display(fmt = "{:?}", self)]
pub struct AnonTarget {
    /// Not necessarily a "real" target label that actually exists, but could be.
    name: TargetLabel,
    /// The type of the rule we are running.
    rule_type: Arc<StarlarkRuleType>,
    /// The attributes the target was defined with.
    /// We use a sorted map since we want to iterate in a defined order.
    attrs: SortedMap<String, ConfiguredAttr>,
    /// The hash of the `rule_type` and `attrs`
    hash: String,
    /// The execution configuration - same as the parent.
    exec_cfg: Configuration,
}

impl ToProtoMessage for AnonTarget {
    type Message = buck2_data::AnonTarget;

    fn as_proto(&self) -> Self::Message {
        buck2_data::AnonTarget {
            name: Some(self.name.as_proto()),
            execution_configuration: Some(self.exec_cfg.as_proto()),
            hash: self.hash.clone(),
        }
    }
}

impl AnonTarget {
    fn mk_hash(rule_type: &StarlarkRuleType, attrs: &SortedMap<String, ConfiguredAttr>) -> String {
        // This is the same hasher as we use for Configuration, so is probably fine.
        // But quite possibly should be a crypto hasher in future.
        let mut hasher = DefaultHasher::new();
        rule_type.hash(&mut hasher);
        attrs.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    pub fn new(
        rule_type: Arc<StarlarkRuleType>,
        name: TargetLabel,
        attrs: SortedMap<String, ConfiguredAttr>,
        exec_cfg: Configuration,
    ) -> Self {
        let hash = Self::mk_hash(&rule_type, &attrs);
        Self {
            name,
            rule_type,
            attrs,
            hash,
            exec_cfg,
        }
    }

    pub fn rule_type(&self) -> &Arc<StarlarkRuleType> {
        &self.rule_type
    }

    pub fn name(&self) -> &TargetLabel {
        &self.name
    }

    pub fn attrs(&self) -> &SortedMap<String, ConfiguredAttr> {
        &self.attrs
    }

    pub fn rule_type_attrs_hash(&self) -> &str {
        &self.hash
    }

    pub fn exec_cfg(&self) -> &Configuration {
        &self.exec_cfg
    }

    pub fn configured_label(&self) -> ConfiguredTargetLabel {
        // We need a configured label, but we don't have a real configuration (because it doesn't make sense),
        // so create a dummy version
        self.name().configure(Configuration::unspecified())
    }
}
