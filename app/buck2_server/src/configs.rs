/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_common::legacy_configs::configs::CellConfigDiff;
use buck2_common::legacy_configs::configs::ConfigDiffEntry;
use buck2_common::legacy_configs::configs::ConfigDiffMetrics;

pub(crate) fn buck_configs(metrics: Option<ConfigDiffMetrics>) -> Vec<buck2_data::CellConfigDiff> {
    let Some(metrics) = metrics else {
        return Vec::new();
    };

    metrics.0.into_iter().map(diff_by_cell).collect()
}

pub(crate) fn diff_by_cell(metrics: CellConfigDiff) -> buck2_data::CellConfigDiff {
    let mut cell_conf = buck2_data::CellConfigDiff {
        config_diff_count: metrics.count as u64,
        config_diff_size: metrics.size_bytes as u64,
        section_diff: HashMap::new(),
    };
    for (section, conf) in metrics.diff {
        let mut section_conf = buck2_data::SectionConfigDiff::default();
        for (name, entry) in conf.0 {
            let (new_value, old_value) = match entry {
                ConfigDiffEntry::Added(new) => (Some(new), None),
                ConfigDiffEntry::Removed(old) => (None, Some(old)),
                ConfigDiffEntry::Changed { new, old } => (Some(new), Some(old)),
            };
            section_conf.config_diff.insert(
                name,
                buck2_data::ConfigDiff {
                    old_value,
                    new_value,
                },
            );
        }
        cell_conf.section_diff.insert(section, section_conf);
    }
    cell_conf
}

#[cfg(test)]
mod tests {
    use buck2_common::legacy_configs::configs::CellConfigDiff;
    use buck2_common::legacy_configs::configs::SectionConfigDiff;
    use maplit::hashmap;
    use starlark_map::smallmap;

    use super::*;

    #[test]
    fn test_buck_configs_without_metrics() {
        assert!(buck_configs(None).is_empty());
    }

    #[test]
    fn test_buck_configs() {
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2_1 = "value2";
        let value2_2 = "value3";
        let section = "apple";

        let metrics = CellConfigDiff {
            diff: smallmap![
                section.to_owned() => SectionConfigDiff(smallmap![
                    key1.to_owned() => ConfigDiffEntry::Removed(value1.to_owned()),
                    key2.to_owned() => ConfigDiffEntry::Changed {
                        new: value2_1.to_owned(),
                        old: value2_2.to_owned()
                    },
                ])
            ],
            count: 2,
            size_bytes: 10,
            diff_size_exceeded: false,
        };
        let metrics = diff_by_cell(metrics);

        let expected = buck2_data::CellConfigDiff {
            config_diff_count: 2,
            config_diff_size: 10,
            section_diff: hashmap![
                section.to_owned() => buck2_data::SectionConfigDiff {
                    config_diff: hashmap![
                        key1.to_owned() => buck2_data::ConfigDiff {
                            old_value: Some(value1.to_owned()),
                            new_value: None,
                        },
                        key2.to_owned() => buck2_data::ConfigDiff {
                            old_value: Some(value2_2.to_owned()),
                            new_value: Some(value2_1.to_owned()),
                        },
                    ]
                }
            ],
        };

        assert_eq!(metrics, expected);
    }
}
