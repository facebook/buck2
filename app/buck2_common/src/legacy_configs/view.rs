/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::str::FromStr;
use std::sync::Arc;

use crate::legacy_configs::LegacyBuckConfig;

/// Buckconfig trait.
///
/// There are two implementations:
/// * simple implementation which is backed by a buckconfig object, used in tests
/// * DICE-backed implementation which records a dependency on buckconfig property in DICE
pub trait LegacyBuckConfigView: Debug {
    fn get(&mut self, section: &str, key: &str) -> anyhow::Result<Option<Arc<str>>>;

    fn parse<T: FromStr>(&mut self, section: &str, key: &str) -> anyhow::Result<Option<T>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        LegacyBuckConfig::parse_value(section, key, self.get(section, key)?.as_deref())
    }

    fn parse_list<T: FromStr>(&mut self, section: &str, key: &str) -> anyhow::Result<Option<Vec<T>>>
    where
        anyhow::Error: From<<T as FromStr>::Err>,
    {
        LegacyBuckConfig::parse_list_value(section, key, self.get(section, key)?.as_deref())
    }
}
