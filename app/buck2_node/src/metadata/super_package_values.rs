/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Debug;

use allocative::Allocative;
use starlark_map::small_map::SmallMap;

use crate::metadata::key::MetadataKey;
use crate::metadata::key::MetadataKeyRef;

pub trait SuperPackageValues: Debug + Allocative + Any + Send + Sync + 'static {
    fn as_any(&self) -> &dyn Any;
    fn is_empty(&self) -> bool;
    /// Only error is internal error: all values must be serializable to JSON,
    /// validated at construction time.
    fn package_values_json(&self) -> buck2_error::Result<SmallMap<MetadataKey, serde_json::Value>>;
    fn contains_key(&self, key: &MetadataKeyRef) -> bool;
    /// Gets package value as a JSON for the given key.
    /// Only error is internal error: all values must be serializable to JSON,
    /// validated at construction time.
    fn get_package_value_json(
        &self,
        key: &MetadataKeyRef,
    ) -> buck2_error::Result<Option<serde_json::Value>>;
}
