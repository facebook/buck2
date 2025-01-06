/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::Mutex;

use buck2_core::cells::name::CellName;
use buck2_error::conversion::from_any;
use buck2_events::dispatch::get_dispatcher;
use dice::DiceComputations;
use dice::UserComputationData;
use dupe::Dupe;
use itertools::Itertools;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::configs::LegacyBuckConfigSection;
use crate::legacy_configs::key::BuckconfigKeyRef;

/// This is a helper struct to track the config diffs between two commands.
///
/// This type is stored in the `UserComputationData` - at the beginning of each command, a new one
/// is created by promoting the one from the previous command. Whenever a cell's buckconfigs are
/// computed, this type is informed and it sends an event to the client.
pub struct ConfigDiffTracker {
    previous: HashMap<CellName, LegacyBuckConfig>,
    current: Mutex<HashMap<CellName, LegacyBuckConfig>>,
    size_limit: Option<usize>,
}

impl ConfigDiffTracker {
    pub fn promote_into(
        previous: &mut DiceComputations<'_>,
        next: &mut UserComputationData,
        root_config: &LegacyBuckConfig,
    ) {
        // `None` indicates that this is the first command
        let previous = match previous.per_transaction_data().data.get::<Self>() {
            Ok(previous) => {
                // It's not enough to just take the current set from the previous command, because
                // the previous command might not have computed some of the buckconfigs - maybe
                // because it didn't need them, or maybe because they didn't change. If we didn't
                // compute them previously, the right thing to do is to assume that the most recent
                // ones that were computed are still good, and diff against those next command.
                let mut new = previous.previous.clone();
                new.extend(
                    previous
                        .current
                        .lock()
                        .unwrap()
                        .iter()
                        .map(|(x, y)| (*x, y.dupe())),
                );
                new
            }
            Err(_) => HashMap::new(),
        };

        // We parse this here, instead of doing it dynamically, to ensure that we don't take a
        // dependency on the root config from every other config
        let size_limit = root_config
            .parse(BuckconfigKeyRef {
                section: "buck2",
                property: "config_diff_size_limit",
            })
            .map_err(from_any)
            // FIXME(JakobDegen): Don't ignore errors
            .unwrap_or_default();

        let val = ConfigDiffTracker {
            previous,
            current: Mutex::new(HashMap::new()),
            size_limit,
        };

        next.data.set(val);
    }

    pub(crate) fn report_computed_config(
        ctx: &mut DiceComputations<'_>,
        cell: CellName,
        config: &LegacyBuckConfig,
    ) {
        let Ok(this) = ctx.per_transaction_data().data.get::<Self>() else {
            // This can happen in tests
            return;
        };

        if this
            .current
            .lock()
            .unwrap()
            .try_insert(cell, config.dupe())
            .is_err()
        {
            // This is a bit suspicious, we normally should not compute the same key twice. It does
            // mean that the diff was already reported though, so doing nothing seems safe
            return;
        }

        let event = if let Some(previous) = this.previous.get(&cell) {
            CellConfigDiff::new(Some(previous), Some(config), &this.size_limit).inner
        } else {
            // If there is no previous set, that usually means that this is a new daemon, or maybe
            // that this particular cell was not loaded in the previous command. To avoid generating
            // a very large diff, we do not report this to the client - but we still need to tell
            // the client that there were some new configs
            buck2_data::CellConfigDiff {
                new_config_indicator_only: true,
                ..Default::default()
            }
        };

        get_dispatcher().instant_event(event);
    }
}

// section name to config diffs
#[derive(Debug, Clone, Default, PartialEq)]
struct CellConfigDiff {
    inner: buck2_data::CellConfigDiff,
    diff_size_exceeded: bool,
}

impl CellConfigDiff {
    fn new(
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
                this.inner.section_diff.insert(section.to_owned(), diff);
            }
        }

        this
    }

    fn section_diff(
        &mut self,
        new: Option<&LegacyBuckConfigSection>,
        old: Option<&LegacyBuckConfigSection>,
        diff_size_limit: &Option<usize>,
    ) -> Option<buck2_data::SectionConfigDiff> {
        let mut result = HashMap::new();
        let empty = SortedMap::new();
        let new_section = new.map(|n| &n.values).unwrap_or(&empty);
        let old_section = old.map(|o| &o.values).unwrap_or(&empty);

        for (name, new_conf, old_conf) in merge(&new_section, &old_section) {
            let new_conf = new_conf.map(|x| x.as_str());
            let old_conf = old_conf.map(|x| x.as_str());
            if new_conf == old_conf {
                continue;
            }
            self.inner.config_diff_count += 1;
            self.inner.config_diff_size +=
                (name.len() + old_conf.map_or(0, |x| x.len()) + new_conf.map_or(0, |x| x.len()))
                    as u64;
            self.insert_if_fits(
                &mut result,
                name,
                buck2_data::ConfigDiff {
                    old_value: old_conf.map(|x| x.to_owned()),
                    new_value: new_conf.map(|x| x.to_owned()),
                },
                diff_size_limit,
            );
        }

        if result.is_empty() {
            None
        } else {
            Some(buck2_data::SectionConfigDiff {
                config_diff: result,
            })
        }
    }

    fn insert_if_fits(
        &mut self,
        map: &mut HashMap<String, buck2_data::ConfigDiff>,
        name: &String,
        entry: buck2_data::ConfigDiff,
        diff_size_limit: &Option<usize>,
    ) {
        if let Some(limit) = diff_size_limit {
            if self.inner.config_diff_size < (*limit) as u64 {
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
    use maplit::hashmap;
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
    fn test_diff_metrics_equal_configs() -> buck2_error::Result<()> {
        let config_args = vec![ConfigOverride::flag_no_cell("apple.key=value1")];
        let config = parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args)?;

        let metrics = CellConfigDiff::new(Some(&config), Some(&config), &Some(10000));

        assert_eq!(metrics.inner.config_diff_count, 0);
        assert_eq!(metrics.inner.config_diff_size, 0);
        assert_eq!(metrics.inner.section_diff, HashMap::new());
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_with_empty() -> buck2_error::Result<()> {
        let key = "key";
        let value = "value1";
        let limit_key = "config_diff_size_limit";
        let limit_value = "10000";
        let config_args = vec![
            ConfigOverride::flag_no_cell(&format!("buck2.{limit_key}={limit_value}")),
            ConfigOverride::flag_no_cell(&format!("apple.{key}={value}")),
        ];
        let config = parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args)?;

        let metrics = CellConfigDiff::new(Some(&config), None, &Some(10000));

        assert_eq!(metrics.inner.config_diff_count, 2);
        assert_eq!(
            metrics.inner.config_diff_size as usize,
            key.len() + value.len() + limit_key.len() + limit_value.len()
        );
        let expected = hashmap![
            "apple".to_owned() => buck2_data::SectionConfigDiff {
                config_diff: hashmap![
                    key.to_owned() => buck2_data::ConfigDiff {
                        old_value: None,
                        new_value: Some(value.to_owned()),
                    },
                ],
            },
            "buck2".to_owned() => buck2_data::SectionConfigDiff {
                config_diff: hashmap![
                    limit_key.to_owned() => buck2_data::ConfigDiff {
                        old_value: None,
                        new_value: Some(limit_value.to_owned()),
                    },
                ],
            },
        ];
        assert_eq!(metrics.inner.section_diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_only_changed() -> buck2_error::Result<()> {
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2_1 = "value2";
        let value2_2 = "value3";
        let key3 = "key3";
        let value3 = "value3";

        let config_args1 = vec![
            ConfigOverride::flag_no_cell(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag_no_cell(&format!("apple.{key2}={value2_1}")),
        ];
        let config1 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args1)?;

        let config_args2 = vec![
            ConfigOverride::flag_no_cell(&format!("apple.{key1}={value1}")),
            ConfigOverride::flag_no_cell(&format!("apple.{key2}={value2_2}")),
            ConfigOverride::flag_no_cell(&format!("apple.{key3}={value3}")),
        ];
        let config2 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args2)?;

        let metrics = CellConfigDiff::new(Some(&config1), Some(&config2), &Some(1000));

        assert_eq!(metrics.inner.config_diff_count, 2);
        assert_eq!(
            metrics.inner.config_diff_size as usize,
            key2.len() + value2_1.len() + value2_2.len() + key3.len() + value3.len()
        );

        let expected = hashmap![
            "apple".to_owned() => buck2_data::SectionConfigDiff {
                config_diff: hashmap![
                    key2.to_owned() => buck2_data::ConfigDiff {
                        old_value: Some(value2_2.to_owned()),
                        new_value: Some(value2_1.to_owned()),
                    },
                    key3.to_owned() => buck2_data::ConfigDiff {
                        old_value: Some(value3.to_owned()),
                        new_value: None
                    },
                ]
            }
        ];
        assert_eq!(metrics.inner.section_diff, expected);
        assert_eq!(metrics.diff_size_exceeded, false);
        Ok(())
    }

    #[test]
    fn test_diff_metrics_size_exceeded() -> buck2_error::Result<()> {
        let key1 = "key1";
        let value1 = "value1";
        let key2 = "key2";
        let value2 = "value2";

        let config_args1 = vec![ConfigOverride::flag_no_cell(&format!(
            "apple.{key1}={value1}"
        ))];
        let config1 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args1)?;

        let config_args2 = vec![ConfigOverride::flag_no_cell(&format!(
            "apple.{key2}={value2}"
        ))];
        let config2 =
            parse_with_config_args(&[("config", indoc!(r#""#))], "config", &config_args2)?;

        let metrics = CellConfigDiff::new(Some(&config1), Some(&config2), &Some(12));

        assert_eq!(metrics.inner.config_diff_count, 2);
        assert_eq!(
            metrics.inner.config_diff_size as usize,
            key1.len() + value1.len() + key2.len() + value2.len()
        );

        let expected = hashmap![
            "apple".to_owned() => buck2_data::SectionConfigDiff {
                config_diff: hashmap![
                    key1.to_owned() => buck2_data::ConfigDiff {
                        old_value: None,
                        new_value: Some(value1.to_owned()),
                    },
                ],
            },
        ];
        assert_eq!(metrics.inner.section_diff, expected);
        assert_eq!(metrics.diff_size_exceeded, true);

        Ok(())
    }
}
