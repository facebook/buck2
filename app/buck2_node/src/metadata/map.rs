/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_core::metadata_key::MetadataKey;
use starlark_map::ordered_map::OrderedMap;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, Eq, PartialEq, Clone, Allocative, Default)]
pub struct MetadataMap {
    values: Box<OrderedMap<MetadataKey, serde_json::Value>>,
}

impl MetadataMap {
    pub fn new(values: OrderedMap<MetadataKey, serde_json::Value>) -> Self {
        Self {
            values: Box::new(values),
        }
    }
}

impl Hash for MetadataMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { values } = self;
        state.write_usize(values.len());
        for (k, v) in values.iter_hashed() {
            Hash::hash(&k, state);
            hash_value(v, state);
        }
    }
}

fn hash_value<H: Hasher>(v: &serde_json::Value, state: &mut H) {
    use serde_json::Value;

    match v {
        Value::Null => {
            state.write_u8(0);
        }
        Value::Bool(v) => {
            state.write_u8(1);
            v.hash(state);
        }
        Value::Number(v) => {
            state.write_u8(2);
            if let Some(v) = v.as_u64() {
                state.write_u8(1);
                v.hash(state);
            } else if let Some(v) = v.as_i64() {
                state.write_u8(2);
                v.hash(state);
            } else {
                state.write_u8(3);
                v.to_string().hash(state);
            }
        }
        Value::String(v) => {
            state.write_u8(3);
            v.hash(state);
        }
        Value::Array(vals) => {
            state.write_u8(4);
            state.write_usize(vals.len());
            for v in vals {
                hash_value(v, state);
            }
        }
        Value::Object(vals) => {
            state.write_u8(5);
            state.write_usize(vals.len());
            for (k, v) in vals {
                k.hash(state);
                hash_value(v, state);
            }
        }
    }
}

impl MetadataMap {
    pub fn to_value(&self) -> serde_json::Value {
        let Self { values } = self;
        let map = values
            .iter()
            .map(|(k, v)| (k.as_str().to_owned(), v.clone()))
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
    fn any_matches(&self, filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        let Self { values } = self;

        for (k, v) in values.iter() {
            if filter(k.as_str())? {
                return Ok(true);
            }
            if v.any_matches(filter)? {
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
        let mut map = OrderedMap::new();
        map.insert(
            "foo.bar".to_owned().try_into().unwrap(),
            serde_json::Value::String("baz".to_owned()),
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
