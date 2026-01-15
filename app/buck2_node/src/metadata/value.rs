/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;

#[derive(
    Debug,
    // serde_json's equality ignores map ordering, but we should have
    // equality with ordering preserved here.
    // TODO: implement our own equality here that preserves ordering
    Eq,
    PartialEq,
    Clone,
    Dupe,
    Allocative,
    Default,
    Serialize,
    Deserialize,
    Pagable
)]
pub struct MetadataValue(pub Arc<serde_json::Value>);

impl MetadataValue {
    pub fn new(value: serde_json::Value) -> Self {
        let value = Arc::new(value);
        Self(value)
    }

    pub fn as_json(&self) -> &serde_json::Value {
        self.0.as_ref()
    }
}

impl Hash for MetadataValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        hash_value(&self.0, state);
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
