/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;
use std::sync::Weak;

use allocative::Allocative;

use crate::api::key::Key;
use crate::api::storage_type::StorageType;
use crate::legacy::incremental::graph::storage_properties::StorageProperties;
use crate::legacy::DiceLegacy;

#[derive(Debug, Allocative)]
#[allocative(bound = "")]
pub(crate) struct StoragePropertiesForKey<K: Key> {
    _k: std::marker::PhantomData<K>,
    pub(crate) dice: Weak<DiceLegacy>,
}

impl<K: Key> StoragePropertiesForKey<K> {
    pub(crate) fn new(dice: &Arc<DiceLegacy>) -> StoragePropertiesForKey<K> {
        StoragePropertiesForKey {
            _k: std::marker::PhantomData,
            dice: Arc::downgrade(dice),
        }
    }
}

impl<K> StorageProperties for StoragePropertiesForKey<K>
where
    K: Key,
{
    type Key = K;
    type Value = K::Value;

    fn storage_type(&self) -> StorageType {
        K::storage_type()
    }

    fn equality(&self, x: &Self::Value, y: &Self::Value) -> bool {
        K::equality(x, y)
    }

    fn validity(&self, x: &Self::Value) -> bool {
        K::validity(x)
    }

    fn key_type_name() -> &'static str {
        K::key_type_name()
    }

    fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
        key
    }
}
