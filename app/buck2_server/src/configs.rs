/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_common::legacy_configs::configs::ConfigDiffEntry;
use buck2_common::legacy_configs::configs::ConfigDiffMetrics;

pub(crate) fn buck_configs(
    new_configs_used: bool,
    metrics: Option<ConfigDiffMetrics>,
) -> buck2_data::BuckConfigs {
    let (config_diff_count, config_diff_size, cell_diff) = match metrics {
        Some(metrics) => (
            Some(metrics.count as u64),
            Some(metrics.size_bytes as u64),
            diff_by_cell(metrics),
        ),

        None => (None, None, HashMap::new()),
    };
    buck2_data::BuckConfigs {
        new_configs_used,
        config_diff_count,
        config_diff_size,
        cell_diff,
    }
}

fn diff_by_cell(metrics: ConfigDiffMetrics) -> HashMap<String, buck2_data::CellConfigDiff> {
    let mut diff = HashMap::new();
    for (cell, conf) in metrics.diff {
        let mut cell_conf = buck2_data::CellConfigDiff::default();
        for (section, conf) in conf.0 {
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
        diff.insert(cell.as_str().to_owned(), cell_conf);
    }
    diff
}

#[cfg(test)]
mod tests {
    use buck2_common::legacy_configs::configs::CellConfigDiff;
    use buck2_common::legacy_configs::configs::SectionConfigDiff;
    use buck2_core::cells::name::CellName;
    use maplit::hashmap;
    use starlark_map::smallmap;

    use super::*;

    #[test]
    fn test_buck_configs_without_metrics() {
        let buck_configs = buck_configs(true, None);

        let expected = buck2_data::BuckConfigs {
            new_configs_used: true,
            config_diff_count: None,
            config_diff_size: None,
            cell_diff: HashMap::new(),
        };

        assert_eq!(buck_configs, expected);
    }

    #[test]
    fn test_buck_configs() {
        let cell_name = "root";
        let cell = CellName::testing_new(cell_name);
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2_1 = "value2";
        let value2_2 = "value3";
        let section = "apple";

        let diff = smallmap![
            cell => CellConfigDiff(smallmap![
                section.to_owned() => SectionConfigDiff(smallmap![
                    key1.to_owned() => ConfigDiffEntry::Removed(value1.to_owned()),
                    key2.to_owned() => ConfigDiffEntry::Changed {
                        new: value2_1.to_owned(),
                        old: value2_2.to_owned()
                    },
                ])
            ])
        ];
        let metrics = ConfigDiffMetrics {
            diff,
            count: 2,
            size_bytes: 10,
            diff_size_exceeded: false,
        };
        let buck_configs = buck_configs(true, Some(metrics));

        let expected = buck2_data::BuckConfigs {
            new_configs_used: true,
            config_diff_count: Some(2),
            config_diff_size: Some(10),
            cell_diff: hashmap![
                cell_name.to_owned() => buck2_data::CellConfigDiff {
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
                    ]
                }
            ],
        };

        assert_eq!(buck_configs, expected);
    }
}
