/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt;
use std::hash::Hash;

use allocative::Allocative;
use pagable::Pagable;
use starlark_map::small_map::SmallMap;
use starlark_map::sorted_map::SortedMap;

use crate::attrs::attr_type::any_matches::AnyMatches;
use crate::metadata::key::MetadataKey;
use crate::metadata::key::MetadataKeyRef;
use crate::metadata::value::MetadataValue;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative, Default, Pagable)]
pub struct MetadataMap {
    values: Box<SortedMap<MetadataKey, MetadataValue>>,
}

impl MetadataMap {
    pub fn new(values: SmallMap<MetadataKey, MetadataValue>) -> Self {
        Self {
            values: Box::new(SortedMap::from(values)),
        }
    }

    pub fn get(&self, key: &MetadataKeyRef) -> Option<&MetadataValue> {
        self.values.get(key)
    }
}

impl MetadataMap {
    pub fn to_value(&self) -> serde_json::Value {
        let Self { values } = self;
        let map = values
            .iter()
            .map(|(k, v)| (k.as_str().to_owned(), v.as_json().clone()))
            .collect();
        serde_json::Value::Object(map)
    }
}

impl fmt::Display for MetadataMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { values } = self;
        let s = serde_json::to_string(values).map_err(|_| fmt::Error)?;
        f.write_str(&s)?;
        Ok(())
    }
}

impl AnyMatches for MetadataMap {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        let Self { values } = self;

        for (k, v) in values.iter() {
            if filter(k.as_str())? {
                return Ok(true);
            }
            if v.0.any_matches(filter)? {
                return Ok(true);
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make() -> MetadataMap {
        let mut map = SmallMap::new();
        map.insert(
            "foo.bar".to_owned().try_into().unwrap(),
            MetadataValue::new(serde_json::Value::String("baz".to_owned())),
        );
        MetadataMap::new(map)
    }

    #[test]
    fn test_display() {
        let spec = make();
        assert_eq!(spec.to_string(), "{\"foo.bar\":\"baz\"}");
    }

    #[test]
    fn test_to_value() {
        let spec = make();
        assert_eq!(spec.to_value(), serde_json::json!({"foo.bar": "baz"}));
    }

    #[test]
    fn test_any_matches() {
        // NOTE: We have more comprehensive tests for serde_json's any_matches, so only test our
        // own map's role here.
        let spec = make();
        assert!(spec.any_matches(&|s| Ok(s == "foo.bar")).unwrap());
        assert!(spec.any_matches(&|s| Ok(s == "baz")).unwrap());
        assert!(!spec.any_matches(&|_s| Ok(false)).unwrap());
    }
}
