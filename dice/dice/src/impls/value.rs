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
use std::fmt::Formatter;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;

use crate::Key;

/// Type erased value associated for each Key in Dice
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceValue(pub(crate) Arc<dyn DiceValueDyn>);

impl Debug for DiceValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceValue").finish_non_exhaustive()
    }
}

impl DiceValue {
    pub(crate) fn new<V: DiceValueDyn>(value: V) -> DiceValue {
        DiceValue(Arc::new(value))
    }

    pub(crate) fn downcast_ref<V: Any>(&self) -> Option<&V> {
        self.0.downcast_ref()
    }

    /// Dynamic version of `Key::equality`.
    pub(crate) fn equality(&self, other: &DiceValue) -> bool {
        self.0.equality(&*other.0)
    }
}

pub(crate) trait DiceValueDyn: Allocative + Any + Send + Sync + 'static {
    fn value_as_any(&self) -> &dyn Any;
    /// Panics if called with incompatible values.
    fn equality(&self, other: &dyn DiceValueDyn) -> bool;
    fn validity(&self) -> bool;
}

impl dyn DiceValueDyn {
    pub(crate) fn downcast_ref<V: Any>(&self) -> Option<&V> {
        self.value_as_any().downcast_ref()
    }
}

#[derive(Allocative)]
pub(crate) struct DiceComputedValue<K: Key> {
    value: K::Value,
}

impl<K> DiceComputedValue<K>
where
    K: Key,
{
    pub(crate) fn new(value: K::Value) -> Self {
        Self { value }
    }
}

impl<K> DiceValueDyn for DiceComputedValue<K>
where
    K: Key,
{
    fn value_as_any(&self) -> &dyn Any {
        &self.value
    }

    fn equality(&self, other: &dyn DiceValueDyn) -> bool {
        K::equality(&self.value, other.downcast_ref().unwrap())
    }

    fn validity(&self) -> bool {
        K::validity(&self.value)
    }
}
