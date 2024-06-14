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
use std::str::FromStr;
use std::sync::Arc;

use anyhow::Context;
use buck2_core::cells::name::CellName;
use dupe::Dupe;
use gazebo::eq_chain;
use itertools::Itertools;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;

use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::legacy_configs::CellConfigDiff;
use crate::legacy_configs::ConfigDiffEntry;
use crate::legacy_configs::ConfigDiffMetrics;
use crate::legacy_configs::ConfigValue;
use crate::legacy_configs::LegacyBuckConfig;
use crate::legacy_configs::LegacyBuckConfigSection;
use crate::legacy_configs::LegacyBuckConfigValue;
use crate::legacy_configs::LegacyBuckConfigView;
use crate::legacy_configs::LegacyBuckConfigs;
use crate::legacy_configs::SectionConfigDiff;

#[derive(buck2_error::Error, Debug)]
enum ConfigValueError {
    #[error(
        "Invalid value for buckconfig `{section}.{key}`: conversion to {ty} failed, value as `{value}`"
    )]
    ParseFailed {
        section: String,
        key: String,
        value: String,
        ty: &'static str,
    },
    #[error("Unknown cell: `{0}`")]
    UnknownCell(CellName),
}

impl LegacyBuckConfigs {
    pub fn get<'a>(&'a self, cell_name: CellName) -> anyhow::Result<&'a LegacyBuckConfig> {
        self.data
            .get(&cell_name)
            .ok_or_else(|| ConfigValueError::UnknownCell(cell_name).into())
    }

    pub fn iter(&self) -> impl Iterator<Item = (CellName, &LegacyBuckConfig)> {
        self.data.iter().map(|(name, config)| (*name, config))
    }

    pub fn compare(&self, other: &Self) -> bool {
        let x = &self.data;
        let y = &other.data;

        eq_chain! {
            x.len() == y.len(),
            x.iter().all(|(cell, config)| {
                y.get(cell).map_or(false, |y_config| y_config.compare(config))
            })
        }
    }
}

impl LegacyBuckConfigView for &LegacyBuckConfig {
    fn get(&mut self, key: BuckconfigKeyRef) -> anyhow::Result<Option<Arc<str>>> {
        Ok(LegacyBuckConfig::get(self, key).map(|v| v.to_owned().into()))
    }
}

impl LegacyBuckConfigSection {
    /// configs are equal if the data they resolve in is equal, regardless of the origin of the config
    pub(crate) fn compare(&self, other: &Self) -> bool {
        eq_chain!(
            self.values.len() == other.values.len(),
            self.values.iter().all(|(name, value)| other
                .values
                .get(name)
                .map_or(false, |other_val| other_val.as_str() == value.as_str()))
        )
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, LegacyBuckConfigValue)> {
        self.values
            .iter()
            .map(move |(key, value)| (key.as_str(), LegacyBuckConfigValue { value }))
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.values.keys()
    }

    pub fn get(&self, key: &str) -> Option<LegacyBuckConfigValue> {
        self.values
            .get(key)
            .map(move |value| LegacyBuckConfigValue { value })
    }
}

impl LegacyBuckConfig {
    fn get_config_value(&self, key: BuckconfigKeyRef) -> Option<&ConfigValue> {
        let BuckconfigKeyRef { section, property } = key;
        self.0
            .values
            .get(section)
            .and_then(|s| s.values.get(property))
    }

    pub fn get(&self, key: BuckconfigKeyRef) -> Option<&str> {
        self.get_config_value(key).map(|s| s.as_str())
    }

    /// Iterate all entries.
    pub fn iter(&self) -> impl Iterator<Item = (&str, impl IntoIterator<Item = (&str, &str)>)> {
        self.0.values.iter().map(|(section, section_values)| {
            (
                section.as_str(),
                section_values
                    .values
                    .iter()
                    .map(|(key, value)| (key.as_str(), value.as_str())),
            )
        })
    }

    fn parse_impl<T: FromStr>(key: BuckconfigKeyRef, value: &str) -> anyhow::Result<T>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        let BuckconfigKeyRef { section, property } = key;
        value
            .parse()
            .map_err(anyhow::Error::from)
            .with_context(|| ConfigValueError::ParseFailed {
                section: section.to_owned(),
                key: property.to_owned(),
                value: value.to_owned(),
                ty: std::any::type_name::<T>(),
            })
    }

    pub fn parse<T: FromStr>(&self, key: BuckconfigKeyRef) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        self.get_config_value(key)
            .map(|s| {
                Self::parse_impl(key, s.as_str()).with_context(|| {
                    format!("Defined {}", s.source.as_legacy_buck_config_location())
                })
            })
            .transpose()
    }

    pub fn parse_value<T: FromStr>(
        key: BuckconfigKeyRef,
        value: Option<&str>,
    ) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        value.map(|s| Self::parse_impl(key, s)).transpose()
    }

    pub fn parse_list<T: FromStr>(&self, key: BuckconfigKeyRef) -> anyhow::Result<Option<Vec<T>>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        Self::parse_list_value(key, self.get(key))
    }

    pub fn parse_list_value<T: FromStr>(
        key: BuckconfigKeyRef,
        value: Option<&str>,
    ) -> anyhow::Result<Option<Vec<T>>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        /// A wrapper type so we can use .parse() on this.
        struct ParseList<T>(Vec<T>);

        impl<T> FromStr for ParseList<T>
        where
            T: FromStr,
        {
            type Err = <T as FromStr>::Err;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                Ok(Self(
                    s.split(',').map(T::from_str).collect::<Result<_, _>>()?,
                ))
            }
        }

        Ok(Self::parse_value::<ParseList<T>>(key, value)?.map(|l| l.0))
    }

    pub fn sections(&self) -> impl Iterator<Item = &String> {
        self.0.values.keys()
    }

    pub fn all_sections(&self) -> impl Iterator<Item = (&String, &LegacyBuckConfigSection)> + '_ {
        self.0.values.iter()
    }

    pub fn get_section(&self, section: &str) -> Option<&LegacyBuckConfigSection> {
        self.0.values.get(section)
    }

    /// configs are equal if the data they resolve in is equal, regardless of the origin of the config
    pub(crate) fn compare(&self, other: &Self) -> bool {
        eq_chain!(
            self.0.values.len() == other.0.values.len(),
            self.0.values.iter().all(|(section_name, section)| {
                other
                    .0
                    .values
                    .get(section_name)
                    .map_or(false, |other_sec| other_sec.compare(section))
            })
        )
    }
}

impl ConfigDiffMetrics {
    pub fn new(root_cell: CellName, new: &LegacyBuckConfigs, old: &LegacyBuckConfigs) -> Self {
        let mut metrics = Self::default();
        let diff_size_limit: Option<usize> = new
            .get(root_cell)
            .ok()
            .map(|root_conf| {
                root_conf
                    .parse(BuckconfigKeyRef {
                        section: "buck2",
                        property: "config_diff_size_limit",
                    })
                    .unwrap_or_default()
            })
            .flatten();

        for (cell, new_config, old_config) in merge(&new.data, &old.data) {
            if let Some(diff) = metrics.cell_diff(new_config, old_config, &diff_size_limit) {
                metrics.diff.insert(cell.dupe(), diff);
            }
        }

        metrics
    }

    pub fn has_changed(&self) -> bool {
        self.count != 0
    }

    fn cell_diff(
        &mut self,
        new: Option<&LegacyBuckConfig>,
        old: Option<&LegacyBuckConfig>,
        diff_size_limit: &Option<usize>,
    ) -> Option<CellConfigDiff> {
        let mut result = SmallMap::new();
        let empty = SortedMap::new();
        let new_conf = new.map(|n| &n.0.values).unwrap_or(&empty);
        let old_conf = old.map(|o| &o.0.values).unwrap_or(&empty);

        for (section, new_conf, old_conf) in merge(&new_conf, &old_conf) {
            if let Some(diff) = self.section_diff(new_conf, old_conf, diff_size_limit) {
                result.insert(section.to_owned(), diff);
            }
        }

        if result.is_empty() {
            None
        } else {
            Some(CellConfigDiff(result))
        }
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
    use starlark_map::sorted_map::SortedMap;

    use super::merge;

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
}
