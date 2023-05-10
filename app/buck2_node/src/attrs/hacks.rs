/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Utility functions to introspect coerced values. This will go away once we have more of the value
//! coercion tooling done. For now, it handles things like stringification for the `targets` command,
//! converting to JSON, etc.

use buck2_core::package::PackageLabel;

use crate::attrs::coerced_attr::CoercedAttr;
use crate::attrs::fmt_context::AttrFmtContext;

pub fn value_to_json(value: &CoercedAttr, pkg: PackageLabel) -> anyhow::Result<serde_json::Value> {
    value.to_json(&AttrFmtContext { package: Some(pkg) })
}

pub fn value_to_string(value: &CoercedAttr, pkg: PackageLabel) -> anyhow::Result<String> {
    match value_to_json(value, pkg)?.as_str() {
        Some(s) => Ok(s.to_owned()),
        None => Err(anyhow::Error::msg("Expected a string, did not get one")),
    }
}
