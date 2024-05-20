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

use crate::arc::Arc;
use crate::versions::VersionRanges;
use crate::Key;
use crate::ProjectionKey;

/// Type erased value associated for each Key in Dice. The 'DiceValidValue' only holds valid values
/// and never anything that is transient, or whose dependencies are transient.
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceValidValue(std::sync::Arc<dyn DiceValueDyn>);

impl Debug for DiceValidValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceValue").finish_non_exhaustive()
    }
}

impl DiceValidValue {
    #[cfg(test)]
    pub(crate) fn downcast_ref<V: Any>(&self) -> Option<&V> {
        self.0.downcast_ref()
    }

    /// Dynamic version of `Key::equality`.
    pub(crate) fn equality(&self, other: &DiceValidValue) -> bool {
        self.0.equality(&*other.0)
    }
}

/// Type erased value that may be transient, or whose dependencies are transient
#[derive(Allocative, Clone, Dupe)]
pub(crate) struct MaybeValidDiceValue {
    value: std::sync::Arc<dyn DiceValueDyn>,
    validity: DiceValidity,
}

impl MaybeValidDiceValue {
    pub(crate) fn new(
        value: std::sync::Arc<dyn DiceValueDyn>,
        deps_validity: DiceValidity,
    ) -> Self {
        let validity = if value.validity() {
            deps_validity
        } else {
            DiceValidity::Transient
        };

        Self { value, validity }
    }

    pub(crate) fn valid(value: DiceValidValue) -> Self {
        Self {
            value: value.0,
            validity: DiceValidity::Valid,
        }
    }

    #[cfg(test)]
    pub(crate) fn transient(value: std::sync::Arc<dyn DiceValueDyn>) -> Self {
        Self {
            value,
            validity: DiceValidity::Transient,
        }
    }

    pub(crate) fn validity(&self) -> DiceValidity {
        self.validity
    }

    pub(crate) fn downcast_maybe_transient<V: Any>(&self) -> Option<&V> {
        self.value.downcast_ref()
    }

    /// Dynamic version of `Key::equality`.
    #[cfg(test)]
    pub(crate) fn equality(&self, other: &DiceValidValue) -> bool {
        self.value.equality(&*other.0)
    }

    #[cfg(test)]
    pub(crate) fn instance_equal(&self, other: &DiceValidValue) -> bool {
        #[allow(ambiguous_wide_pointer_comparisons)]
        // we literally just want to compare the exact pointer
        std::sync::Arc::ptr_eq(&self.value, &other.0)
    }

    pub(crate) fn into_valid_value(self) -> Result<DiceValidValue, MaybeValidDiceValue> {
        match self.validity {
            DiceValidity::Valid => Ok(DiceValidValue(self.value)),
            DiceValidity::Transient => Err(self),
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

impl DiceValidity {
    pub(crate) fn and(&mut self, other: Self) {
        if other == DiceValidity::Transient {
            *self = DiceValidity::Transient;
        }
    }
}

#[derive(Allocative, Clone)]
pub(crate) struct DiceComputedValue {
    value: MaybeValidDiceValue,
    valid: Arc<VersionRanges>,
}

impl Dupe for DiceComputedValue {
    // triomphe Arc is dupe
}

impl DiceComputedValue {
    pub(crate) fn new(value: MaybeValidDiceValue, valid: Arc<VersionRanges>) -> Self {
        Self { value, valid }
    }

    pub(crate) fn value(&self) -> &MaybeValidDiceValue {
        &self.value
    }

    pub(crate) fn versions(&self) -> &VersionRanges {
        &self.valid
    }
}

impl Debug for DiceComputedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceComputedValue")
            .field("valid", &self.valid)
            .finish_non_exhaustive()
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

#[cfg(test)]
mod testing {
    use std::sync::Arc;

    use crate::impls::value::DiceValidValue;
    use crate::impls::value::DiceValueDyn;
    use crate::impls::value::MaybeValidDiceValue;

    impl DiceValidValue {
        pub(crate) fn testing_new<V: DiceValueDyn>(value: V) -> Self {
            Self(std::sync::Arc::new(value))
        }
    }

    impl MaybeValidDiceValue {
        pub(crate) fn testing_value(&self) -> &Arc<dyn DiceValueDyn> {
            &self.value
        }
    }
}
