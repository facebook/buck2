/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::hash::Hash;

use itertools::Itertools;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::configs::LegacyBuckConfigSection;
use crate::legacy_configs::configs::LegacyBuckConfigs;

#[derive(Debug, Clone, PartialEq)]
pub enum ConfigDiffEntry {
    Added(String),
    Removed(String),
    Changed { new: String, old: String },
}

// config name to config diff
#[derive(Debug, Clone, Default, PartialEq)]
pub struct SectionConfigDiff(pub SmallMap<String, ConfigDiffEntry>);

// section name to config diffs
#[derive(Debug, Clone, Default, PartialEq)]
pub struct CellConfigDiff {
    pub diff: SmallMap<String, SectionConfigDiff>,
    // count of changed/removed/added config antries
    pub count: usize,
    // key + old value + new value
    pub size_bytes: usize,
    // if diff map is complete or partial due to size limit
    pub diff_size_exceeded: bool,
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct ConfigDiffMetrics(pub Vec<CellConfigDiff>);

impl ConfigDiffMetrics {
    pub fn new(
        new: &LegacyBuckConfigs,
        old: &LegacyBuckConfigs,
        diff_size_limit: &Option<usize>,
    ) -> Self {
        let mut metrics = Self::default();

        for (_, new_config, old_config) in merge(&new.data, &old.data) {
            let diff = CellConfigDiff::new(new_config, old_config, diff_size_limit);
            metrics.0.push(diff);
        }

        metrics
    }
}

impl CellConfigDiff {
    pub(crate) fn new(
        new: Option<&LegacyBuckConfig>,
        old: Option<&LegacyBuckConfig>,
        diff_size_limit: &Option<usize>,
    ) -> CellConfigDiff {
        let mut this = Self::default();
        let empty = SortedMap::new();
        let new_conf = new.map(|n| &n.0.values).unwrap_or(&empty);
        let old_conf = old.map(|o| &o.0.values).unwrap_or(&empty);

        for (section, new_conf, old_conf) in merge(&new_conf, &old_conf) {
            if let Some(diff) = this.section_diff(new_conf, old_conf, diff_size_limit) {
                this.diff.insert(section.to_owned(), diff);
            }
        }

        this
    }

    fn section_diff(
        &mut self,
        new: Option<&LegacyBuckConfigSection>,
        old: Option<&LegacyBuckConfigSection>,
        diff_size_limit: &Option<usize>,
    ) -> Option<SectionConfigDiff> {
        let mut result = SmallMap::new();
        let empty = SortedMap::new();
        let new_section = new.map(|n| &n.values).unwrap_or(&empty);
        let old_section = old.map(|o| &o.values).unwrap_or(&empty);

        for (name, new_conf, old_conf) in merge(&new_section, &old_section) {
            match (new_conf, old_conf) {
                (None, Some(old_value)) => {
                    let old_value = old_value.as_str();
                    self.count += 1;
                    self.size_bytes += name.len() + old_value.len();
                    self.insert_if_fits(
                        &mut result,
                        name,
                        ConfigDiffEntry::Removed(old_value.to_owned()),
                        diff_size_limit,
                    );
                }
                (Some(new_value), None) => {
                    let new_value = new_value.as_str();
                    self.count += 1;
                    self.size_bytes += name.len() + new_value.len();
                    self.insert_if_fits(
                        &mut result,
                        name,
                        ConfigDiffEntry::Added(new_value.to_owned()),
                        diff_size_limit,
                    );
                }
                (Some(new_value), Some(old_value)) => {
                    let new_value = new_value.as_str();
                    let old_value = old_value.as_str();
                    if new_value != old_value {
                        self.count += 1;
                        self.size_bytes += name.len() + new_value.len() + old_value.len();
                        self.insert_if_fits(
                            &mut result,
                            name,
                            ConfigDiffEntry::Changed {
                                new: new_value.to_owned(),
                                old: old_value.to_owned(),
                            },
                            diff_size_limit,
                        );
                    }
                }
                (None, None) => {}
            };
        }

        if result.is_empty() {
            None
        } else {
            Some(SectionConfigDiff(result))
        }
    }

    fn insert_if_fits(
        &mut self,
        map: &mut SmallMap<String, ConfigDiffEntry>,
        name: &String,
        entry: ConfigDiffEntry,
        diff_size_limit: &Option<usize>,
    ) {
        if let Some(limit) = diff_size_limit {
            if self.size_bytes < *limit {
                map.insert(name.to_owned(), entry);
            } else {
                self.diff_size_exceeded = true;
            }
        }
    }
}
/// produces ordered elements of a two maps merged together by the key
fn merge<'a, K, V>(
    map0: &'a SortedMap<K, V>,
    map1: &'a SortedMap<K, V>,
) -> impl Iterator<Item = (&'a K, Option<&'a V>, Option<&'a V>)>
where
    K: Ord + Hash,
{
    map0.iter()
        .map(|(k, v)| (k, Some(v), None))
        .merge_by(map1.iter().map(|(k, v)| (k, None, Some(v))), |k0, k1| {
            k0.0 <= k1.0
        })
        .coalesce(|x, y| match x.0.cmp(y.0) {
            Ordering::Less => Err((x, y)),
            Ordering::Equal => Ok((x.0, x.1, y.2)),
            Ordering::Greater => unreachable!("should be sorted"),
        })
}

#[cfg(test)]
mod tests {
    use buck2_cli_proto::ConfigOverride;
    use indoc::indoc;
    use starlark_map::smallmap;
    use starlark_map::sorted_map::SortedMap;

    use super::merge;
    use super::*;
    use crate::legacy_configs::configs::testing::*;

    #[test]
    fn test_merge_empty() {
        let empty1: SortedMap<u8, u8> = SortedMap::new();
        let empty2: SortedMap<u8, u8> = SortedMap::new();

        let expected: Vec<(&u8, Option<&u8>, Option<&u8>)> = vec![];
        let actual: Vec<(&u8, Option<&u8>, Option<&u8>)> = merge(&empty1, &empty2).collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_merge_map_vs_empty() {
        let key = "str";
        let value = 2;
        let map: SortedMap<&str, u8> = SortedMap::from_iter([(key, value)]);
        let empty: SortedMap<&str, u8> = SortedMap::new();

        let expected = vec![(&key, Some(&value), None)];
        let actual: Vec<_> = merge(&map, &empty).collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_merge_empty_vs_map() {
        let key = "str";
        let value = 2;
        let map: SortedMap<&str, u8> = SortedMap::from_iter([(key, value)]);
        let empty: SortedMap<&str, u8> = SortedMap::new();

        let expected = vec![(&key, None, Some(&value))];
        let actual: Vec<_> = merge(&empty, &map).collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_merge_map_vs_map_in_order() {
        let key1 = "str";
        let value1_1 = 1;
        let value1_2 = 2;
        let key2 = "str2";
        let value2 = 7;
        let key3 = "str3";
        let value3 = 3;

        let map1: SortedMap<&str, u8> = SortedMap::from_iter([(key1, value1_1), (key3, value3)]);
        let map2: SortedMap<&str, u8> = SortedMap::from_iter([(key1, value1_2), (key2, value2)]);

        let expected = vec![
            (&key1, Some(&value1_1), Some(&value1_2)),
            (&key2, None, Some(&value2)),
            (&key3, Some(&value3), None),
        ];
        let actual: Vec<_> = merge(&map1, &map2).collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_diff_metrics_equal_configs() -> anyhow::Result<()> {
        let config_args = vec![ConfigOverride::flag("apple.key=value1")];
        let config = parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args)?;

        let metrics = CellConfigDiff::new(Some(&config), Some(&config), &Some(10000));

        assert_eq!(metrics.count, 0);
        assert_eq!(metrics.size_bytes, 0);
        assert_eq!(metrics.diff, SmallMap::new());
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_with_empty() -> anyhow::Result<()> {
        let key = "key";
        let value = "value1";
        let limit_key = "config_diff_size_limit";
        let limit_value = "10000";
        let config_args = vec![
            ConfigOverride::flag(&format!("buck2.{limit_key}={limit_value}")),
            ConfigOverride::flag(&format!("apple.{key}={value}")),
        ];
        let config = parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args)?;

        let metrics = CellConfigDiff::new(Some(&config), None, &Some(10000));

        assert_eq!(metrics.count, 2);
        assert_eq!(
            metrics.size_bytes,
            key.len() + value.len() + limit_key.len() + limit_value.len()
        );
        let expected = smallmap![
            "apple".to_owned() => SectionConfigDiff(smallmap![
                key.to_owned() => ConfigDiffEntry::Added(value.to_owned())
            ]),
            "buck2".to_owned() => SectionConfigDiff(smallmap![
                limit_key.to_owned() => ConfigDiffEntry::Added(limit_value.to_owned())
            ]),
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_only_changed() -> anyhow::Result<()> {
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2_1 = "value2";
        let value2_2 = "value3";
        let key3 = "key3";
        let value3 = "value3";

        let config_args1 = vec![
            ConfigOverride::flag(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag(&format!("apple.{key2}={value2_1}")),
        ];
        let config1 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args1)?;

        let config_args2 = vec![
            ConfigOverride::flag(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag(&format!("apple.{key2}={value2_2}")),
            ConfigOverride::flag(&format!("apple.{key3}={value3}")),
        ];
        let config2 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args2)?;

        let metrics = CellConfigDiff::new(Some(&config1), Some(&config2), &Some(1000));

        assert_eq!(metrics.count, 2);
        assert_eq!(
            metrics.size_bytes,
            key2.len() + value2_1.len() + value2_2.len() + key3.len() + value3.len()
        );

        let expected = smallmap![
            "apple".to_owned() => SectionConfigDiff(smallmap![
                key2.to_owned() => ConfigDiffEntry::Changed {
                    new: value2_1.to_owned(),
                    old: value2_2.to_owned()
                },
                key3.to_owned() => ConfigDiffEntry::Removed(value3.to_owned())
            ])
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_size_exceeded() -> anyhow::Result<()> {
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2 = "value2";

        let config_args1 = vec![ConfigOverride::flag(&format!("apple.{key1}={value1}"))];
        let config1 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args1)?;

        let config_args2 = vec![ConfigOverride::flag(&format!("apple.{key2}={value2}"))];
        let config2 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args2)?;

        let metrics = CellConfigDiff::new(Some(&config1), Some(&config2), &Some(12));

        assert_eq!(metrics.count, 2);
        assert_eq!(
            metrics.size_bytes,
            key1.len() + value1.len() + key2.len() + value2.len()
        );

        let expected = smallmap![
            "apple".to_owned() => SectionConfigDiff(smallmap![
                key1.to_owned() => ConfigDiffEntry::Added(value1.to_owned())
            ])
        ];
        assert_eq!(metrics.diff, expected);
        assert_eq!(metrics.diff_size_exceeded, true);

        Ok(())
    }
}
