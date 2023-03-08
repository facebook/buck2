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

use allocative::Allocative;
use dupe::Dupe;
use triomphe::Arc;

use crate::impls::core::graph::history::CellHistory;
use crate::Key;
use crate::ProjectionKey;

/// Type erased value associated for each Key in Dice
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceValue(pub(crate) std::sync::Arc<dyn DiceValueDyn>);

impl Debug for DiceValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceValue").finish_non_exhaustive()
    }
}

#[allow(unused)]
impl DiceValue {
    pub(crate) fn new<V: DiceValueDyn>(value: V) -> DiceValue {
        DiceValue(std::sync::Arc::new(value))
    }

    pub(crate) fn downcast_ref<V: Any>(&self) -> Option<&V> {
        self.0.downcast_ref()
    }

    /// Dynamic version of `Key::equality`.
    pub(crate) fn equality(&self, other: &DiceValue) -> bool {
        self.0.equality(&*other.0)
    }
}

/// Type erased value associated for each Key in Dice
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceValueWithValidity {
    pub(crate) value: DiceValue,
    pub(crate) validity: DiceValidity,
}

impl Debug for DiceValueWithValidity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.validity {
            DiceValidity::Valid => f.debug_struct("DiceValidValue").finish_non_exhaustive(),
            DiceValidity::Transient => f.debug_struct("DiceTransientValue").finish_non_exhaustive(),
        }
    }
}

/// validity, including based on validity of dependencies
#[derive(Allocative, Clone, Dupe, Copy, PartialEq, Eq, Debug)]
pub(crate) enum DiceValidity {
    Valid,
    /// If the node is invalid or any of its deps are invalid
    Transient,
}

#[derive(Allocative, Clone)]
pub(crate) struct DiceComputedValue {
    value: DiceValue,
    valid: Arc<CellHistory>,
}

impl Dupe for DiceComputedValue {
    // triomphe Arc is dupe
}

impl DiceComputedValue {
    pub(crate) fn new(value: DiceValue, valid: Arc<CellHistory>) -> Self {
        Self { value, valid }
    }

    pub(crate) fn value(&self) -> &DiceValue {
        &self.value
    }

    #[allow(unused)]
    pub(crate) fn history(&self) -> &CellHistory {
        &self.valid
    }
}

impl Debug for DiceComputedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceComputedValue").finish_non_exhaustive()
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
pub(crate) struct DiceKeyValue<K: Key> {
    value: K::Value,
}

impl<K> DiceKeyValue<K>
where
    K: Key,
{
    pub(crate) fn new(value: K::Value) -> Self {
        Self { value }
    }
}

impl<K> DiceValueDyn for DiceKeyValue<K>
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

#[derive(Allocative)]
pub(crate) struct DiceProjectValue<K: ProjectionKey> {
    value: K::Value,
}

impl<K> DiceProjectValue<K>
where
    K: ProjectionKey,
{
    pub(crate) fn new(value: K::Value) -> Self {
        Self { value }
    }
}

impl<K> DiceValueDyn for DiceProjectValue<K>
where
    K: ProjectionKey,
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
