/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use core::fmt;

use buck2_error::buck2_error;
use buck2_hash::BuckHasherBuilder;
use buck2_hash::BuckMutSet;

use crate::configuration::resolved::ConfigurationSettingKey;

#[derive(Debug, Eq, PartialEq)]
pub enum CoercedSelectorKeyRef<'a> {
    Target(&'a ConfigurationSettingKey),
    Default,
}

impl CoercedSelectorKeyRef<'_> {
    pub const DEFAULT_KEY_STR: &'static str = "DEFAULT";
}

impl fmt::Display for CoercedSelectorKeyRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoercedSelectorKeyRef::Target(target) => write!(f, "{}", target.0),
            CoercedSelectorKeyRef::Default => write!(f, "{}", Self::DEFAULT_KEY_STR),
        }
    }
}

/// Rejects duplicate keys in a `select()`, including keys that differ only in
/// how they spell the same label.
pub(crate) fn check_select_keys_unique<V>(
    entries: &[(ConfigurationSettingKey, V)],
) -> buck2_error::Result<()> {
    fn duplicate_key(key: &ConfigurationSettingKey) -> buck2_error::Error {
        buck2_error!(
            buck2_error::ErrorTag::Input,
            "duplicate key `{key}` in `select()`"
        )
    }

    // This is possible when select keys are specified like:
    // ```
    // select({
    //   "cell//foo:bar": 2,
    //   "//foo:bar": 1,
    //   ":bar": 3,
    // })
    // ```
    // Keys are unique strings, but resolved to the same target.

    // Quadratic is cheaper than hashing for small `N`.
    // For 32 entries we do 496 comparisons, which is cheaper than 32 hashing operations.
    if entries.len() <= 32 {
        for i in 0..entries.len() {
            for j in i + 1..entries.len() {
                if entries[i].0 == entries[j].0 {
                    return Err(duplicate_key(&entries[i].0));
                }
            }
        }
    } else {
        let mut visited_keys: BuckMutSet<&ConfigurationSettingKey> =
            BuckMutSet::with_capacity_and_hasher(entries.len(), BuckHasherBuilder);
        for (k, _) in entries {
            if !visited_keys.insert(k) {
                return Err(duplicate_key(k));
            }
        }
    }

    Ok(())
}

pub(crate) fn select_fail_to_json(message: &str) -> Result<serde_json::Value, buck2_error::Error> {
    Ok(serde_json::Value::Object(serde_json::Map::from_iter([
        (
            "__type".to_owned(),
            serde_json::Value::String("select_fail".to_owned()),
        ),
        (
            "message".to_owned(),
            serde_json::Value::String(message.to_owned()),
        ),
    ])))
}

pub(crate) fn select_incompatible_to_json(
    message: &str,
) -> Result<serde_json::Value, buck2_error::Error> {
    Ok(serde_json::Value::Object(serde_json::Map::from_iter([
        (
            "__type".to_owned(),
            serde_json::Value::String("select_incompatible".to_owned()),
        ),
        (
            "message".to_owned(),
            serde_json::Value::String(message.to_owned()),
        ),
    ])))
}

#[cfg(test)]
mod tests {
    use dupe::Dupe;

    use crate::attrs::coerced_attr::check_select_keys_unique;
    use crate::configuration::resolved::ConfigurationSettingKey;

    #[test]
    fn test_check_select_keys_unique_small() {
        let a = (ConfigurationSettingKey::testing_parse("foo//:a"), ());
        let b = (ConfigurationSettingKey::testing_parse("foo//:b"), ());
        let c = (ConfigurationSettingKey::testing_parse("foo//:c"), ());
        assert!(check_select_keys_unique(&[a.dupe(), b.dupe(), a.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a.dupe(), b.dupe(), b.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a.dupe(), a.dupe(), b.dupe()]).is_err());
        assert!(check_select_keys_unique(&[a, b, c]).is_ok());
    }

    #[test]
    fn test_check_select_keys_unique_large() {
        let mut long = (0..100)
            .map(|i| {
                (
                    ConfigurationSettingKey::testing_parse(&format!("foo//:{i}")),
                    (),
                )
            })
            .collect::<Vec<_>>();
        assert!(check_select_keys_unique(&long).is_ok());
        long[10].0 = long[0].0.dupe();
        assert!(check_select_keys_unique(&long).is_err());
    }
}
