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
use buck2_util::strong_hasher::Blake3StrongHasher;
use buck2_util::strong_hasher::StrongHash128;
use dupe::Dupe;
use pagable::Pagable;

use crate::metadata::value::hash_json_value;

pub const MODIFIER_METADATA_KEY: &str = "buck.cfg_modifiers";

/// Config modifiers from a `PACKAGE` file, as JSON.
#[derive(Debug, Clone, Dupe, Allocative, Pagable)]
pub struct PackageCfgModifiersValue {
    json: Arc<serde_json::Value>,
    /// 128 bits of blake3 over the JSON structure, computed once at construction.
    content_hash: StrongHash128,
}

/// `Hash` and `Eq` use only the precomputed content hash so that DICE keys embedding package
/// modifiers hash and compare in O(1) instead of walking the JSON. During execution platform
/// resolution such keys are probed once per (exec dep × candidate platform), which made the
/// JSON walks the dominant cost of cold configuration on graphs with thousands of candidate
/// platforms. Treating 128-bit hash equality as value equality is the same collision tolerance
/// buck2 accepts for content digests.
impl PartialEq for PackageCfgModifiersValue {
    fn eq(&self, other: &Self) -> bool {
        self.content_hash == other.content_hash
    }
}

impl Eq for PackageCfgModifiersValue {}

impl Hash for PackageCfgModifiersValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.content_hash.hash(state);
    }
}

impl PackageCfgModifiersValue {
    pub fn new(v: serde_json::Value) -> Self {
        let mut hasher = Blake3StrongHasher::new();
        hash_json_value(&v, &mut hasher);
        Self {
            json: Arc::new(v),
            content_hash: hasher.finalize128(),
        }
    }

    /// The underlying JSON, shared.
    pub(crate) fn as_json(&self) -> Arc<serde_json::Value> {
        self.json.dupe()
    }

    pub fn json(&self) -> &serde_json::Value {
        &self.json
    }
}
