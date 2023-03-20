/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use allocative::Allocative;

use crate::configuration::constraints::ConstraintKey;
use crate::configuration::constraints::ConstraintValue;

/// Parsed provider returned from `config_setting` rule.
#[derive(Debug, Eq, PartialEq, Hash, Allocative)]
pub struct ConfigSettingData {
    // contains the full specification of the platform configuration
    pub constraints: BTreeMap<ConstraintKey, ConstraintValue>,
    // contains mappings of `section.key` to `value` for buckconfigs
    // TODO(scottcao): Make this into a Vec<ConfigArgumentPair> for more structured data
    // This can't be done right now because ConfigArgumentPair lives in buck2_common
    // and buck2_core cannot depend on buck2_common.
    pub buckconfigs: BTreeMap<String, String>,
}

impl ConfigSettingData {
    fn is_subset<K: Ord, V: Eq>(a: &BTreeMap<K, V>, b: &BTreeMap<K, V>) -> bool {
        // TODO(nga): this can be done in linear time.
        a.len() <= b.len() && a.iter().all(|(k, v)| b.get(k) == Some(v))
    }

    fn len_sum(&self) -> usize {
        self.constraints.len() + self.buckconfigs.len()
    }

    pub fn refines(&self, that: &ConfigSettingData) -> bool {
        self.len_sum() > that.len_sum()
            && Self::is_subset(&that.constraints, &self.constraints)
            && Self::is_subset(&that.buckconfigs, &self.buckconfigs)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use dupe::Dupe;

    use crate::configuration::config_setting::ConfigSettingData;
    use crate::configuration::constraints::ConstraintKey;
    use crate::configuration::constraints::ConstraintValue;
    use crate::target::label::TargetLabel;

    #[test]
    fn is_subset() {
        let m_12_34 = BTreeMap::from_iter([(1, 2), (3, 4)]);
        let m_12_35 = BTreeMap::from_iter([(1, 2), (3, 5)]);
        let m_12 = BTreeMap::from_iter([(1, 2)]);
        let empty = BTreeMap::<u32, u32>::new();

        // A set is a subset of itself
        assert!(ConfigSettingData::is_subset(&m_12_34, &m_12_34));
        assert!(ConfigSettingData::is_subset(&m_12, &m_12));
        assert!(ConfigSettingData::is_subset(&empty, &empty));

        assert!(ConfigSettingData::is_subset(&m_12, &m_12_34));
        assert!(!ConfigSettingData::is_subset(&m_12_34, &m_12));

        assert!(!ConfigSettingData::is_subset(&m_12_34, &m_12_35));
    }

    #[test]
    fn refines() {
        fn constraint_key(t: &str) -> ConstraintKey {
            ConstraintKey(TargetLabel::testing_parse(t))
        }

        fn constraint_value(t: &str) -> ConstraintValue {
            ConstraintValue(TargetLabel::testing_parse(t))
        }

        let os = constraint_key("config//:os");
        let linux = constraint_value("config//:linux");
        let cpu = constraint_key("config//:cpu");
        let arm64 = constraint_value("config//:arm64");
        let x86_64 = constraint_value("config//:x86_64");

        let c_linux = ConfigSettingData {
            constraints: BTreeMap::from_iter([(os.dupe(), linux.dupe())]),
            buckconfigs: BTreeMap::new(),
        };
        let c_arm64 = ConfigSettingData {
            constraints: BTreeMap::from_iter([(cpu.dupe(), arm64.dupe())]),
            buckconfigs: BTreeMap::new(),
        };
        let c_linux_arm64 = ConfigSettingData {
            constraints: BTreeMap::from_iter([
                (os.dupe(), linux.dupe()),
                (cpu.dupe(), arm64.dupe()),
            ]),
            buckconfigs: BTreeMap::new(),
        };
        let c_linux_x86_64 = ConfigSettingData {
            constraints: BTreeMap::from_iter([
                (os.dupe(), linux.dupe()),
                (cpu.dupe(), x86_64.dupe()),
            ]),
            buckconfigs: BTreeMap::new(),
        };

        // Config setting does not refines identical config setting.
        assert!(!c_linux.refines(&c_linux));

        assert!(!c_linux.refines(&c_arm64));
        assert!(!c_arm64.refines(&c_linux));

        assert!(c_linux_arm64.refines(&c_linux));
        assert!(c_linux_arm64.refines(&c_arm64));

        assert!(!c_linux_x86_64.refines(&c_linux_arm64));
    }

    #[test]
    fn buckconfig_refines() {
        let c1 = ConfigSettingData {
            constraints: BTreeMap::new(),
            buckconfigs: BTreeMap::from_iter([("foo.bar".to_owned(), "baz".to_owned())]),
        };
        let c11 = ConfigSettingData {
            constraints: BTreeMap::new(),
            buckconfigs: BTreeMap::from_iter([
                ("foo.bar".to_owned(), "baz".to_owned()),
                ("foo.qux".to_owned(), "quux".to_owned()),
            ]),
        };

        assert!(c11.refines(&c1));
        assert!(!c11.refines(&c11));
        assert!(!c1.refines(&c1));
        assert!(!c1.refines(&c11));
    }
}
