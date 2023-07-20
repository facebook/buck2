/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use allocative::Allocative;

use crate::attrs::attr_type::any_matches::AnyMatches;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Allocative)]
pub struct MetadataMap {
    // TODO: Fill this in.
}

impl MetadataMap {
    pub fn to_value(&self) -> serde_json::Value {
        let Self {} = self; // TODO: Fill this in.
        serde_json::Value::Object(serde_json::Map::new())
    }
}

impl fmt::Display for MetadataMap {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {} = self; // TODO: Fill this in.
        f.write_str("{}")?;
        Ok(())
    }
}

impl AnyMatches for MetadataMap {
    fn any_matches(&self, _filter: &dyn Fn(&str) -> anyhow::Result<bool>) -> anyhow::Result<bool> {
        let Self {} = self; // TODO: Fill this in.
        Ok(false)
    }
}
