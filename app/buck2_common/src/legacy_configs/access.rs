/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str::FromStr;
use std::sync::Arc;

use buck2_error::BuckErrorContext;
use gazebo::eq_chain;

use crate::legacy_configs::configs::ConfigValue;
use crate::legacy_configs::configs::LegacyBuckConfig;
use crate::legacy_configs::configs::LegacyBuckConfigSection;
use crate::legacy_configs::configs::LegacyBuckConfigValue;
use crate::legacy_configs::key::BuckconfigKeyRef;
use crate::legacy_configs::view::LegacyBuckConfigView;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
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
}

impl LegacyBuckConfigView for &LegacyBuckConfig {
    fn get(&mut self, key: BuckconfigKeyRef) -> buck2_error::Result<Option<Arc<str>>> {
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

    fn parse_impl<T: FromStr>(key: BuckconfigKeyRef, value: &str) -> buck2_error::Result<T>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
    {
        let BuckconfigKeyRef { section, property } = key;
        value
            .parse()
            .map_err(buck2_error::Error::from)
            .with_buck_error_context(|| ConfigValueError::ParseFailed {
                section: section.to_owned(),
                key: property.to_owned(),
                value: value.to_owned(),
                ty: std::any::type_name::<T>(),
            })
    }

    pub fn parse<T: FromStr>(&self, key: BuckconfigKeyRef) -> buck2_error::Result<Option<T>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
    {
        self.get_config_value(key)
            .map(|s| {
                Self::parse_impl(key, s.as_str()).with_buck_error_context(|| {
                    format!("Defined {}", s.source.as_legacy_buck_config_location())
                })
            })
            .transpose()
    }

    pub fn parse_value<T: FromStr>(
        key: BuckconfigKeyRef,
        value: Option<&str>,
    ) -> buck2_error::Result<Option<T>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
    {
        value.map(|s| Self::parse_impl(key, s)).transpose()
    }

    pub fn parse_list<T: FromStr>(
        &self,
        key: BuckconfigKeyRef,
    ) -> buck2_error::Result<Option<Vec<T>>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
    {
        Self::parse_list_value(key, self.get(key))
    }

    pub fn parse_list_value<T: FromStr>(
        key: BuckconfigKeyRef,
        value: Option<&str>,
    ) -> buck2_error::Result<Option<Vec<T>>>
    where
        buck2_error::Error: From<<T as FromStr>::Err>,
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
