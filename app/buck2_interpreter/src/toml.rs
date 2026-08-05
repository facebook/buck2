/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Converts TOML to JSON, representing datetimes as strings and non-finite floats as null.
pub fn toml_value_to_json(value: ::toml::Value) -> serde_json::Value {
    match value {
        ::toml::Value::String(s) => serde_json::Value::String(s),
        ::toml::Value::Integer(i) => serde_json::Value::Number(i.into()),
        ::toml::Value::Float(f) => match serde_json::Number::from_f64(f) {
            Some(n) => serde_json::Value::Number(n),
            None => serde_json::Value::Null,
        },
        ::toml::Value::Boolean(b) => serde_json::Value::Bool(b),
        ::toml::Value::Datetime(dt) => serde_json::Value::String(dt.to_string()),
        ::toml::Value::Array(arr) => {
            serde_json::Value::Array(arr.into_iter().map(toml_value_to_json).collect())
        }
        ::toml::Value::Table(table) => serde_json::Value::Object(
            table
                .into_iter()
                .map(|(k, v)| (k, toml_value_to_json(v)))
                .collect(),
        ),
    }
}
