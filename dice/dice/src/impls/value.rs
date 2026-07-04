/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::any::Any;
use std::fmt::Debug;
use std::fmt::Formatter;

use allocative::Allocative;
use dupe::Dupe;
use mini_vec::packed_ptr::PackedPtr;

use crate::Key;
use crate::ProjectionKey;
use crate::api::key::InvalidationSourcePriority;
use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::versions::VersionNumber;
use crate::versions::VersionRanges;

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

    pub(crate) fn as_dyn(&self) -> &dyn DiceValueDyn {
        &*self.0
    }

    pub(crate) fn from_arc(arc: std::sync::Arc<dyn DiceValueDyn>) -> Self {
        Self(arc)
    }
}

/// Type erased value that may be transient, or whose dependencies are transient
#[derive(Allocative, Clone, Dupe)]
pub enum MaybeValidDiceValue {
    Present(std::sync::Arc<dyn DiceValueDyn>),
    Transient(std::sync::Arc<dyn DiceValueDyn>),
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

        match validity {
            DiceValidity::Valid => Self::Present(value),
            DiceValidity::Transient => Self::Transient(value),
        }
    }

    pub(crate) fn valid(value: DiceValidValue) -> Self {
        Self::Present(value.0)
    }

    pub(crate) fn validity(&self) -> DiceValidity {
        match self {
            Self::Transient(_) => DiceValidity::Transient,
            _ => DiceValidity::Valid,
        }
    }

    fn value(&self) -> &std::sync::Arc<dyn DiceValueDyn> {
        match self {
            MaybeValidDiceValue::Present(v) | MaybeValidDiceValue::Transient(v) => v,
        }
    }

    pub(crate) fn downcast_maybe_transient<V: Any>(&self) -> Option<&V> {
        self.value().downcast_ref()
    }

    /// Dynamic version of `Key::equality`.
    #[cfg(test)]
    pub(crate) fn equality(&self, other: &DiceValidValue) -> bool {
        self.value().equality(&*other.0)
    }

    #[cfg(test)]
    pub(crate) fn instance_equal(&self, other: &DiceValidValue) -> bool {
        #[allow(ambiguous_wide_pointer_comparisons)]
        // we literally just want to compare the exact pointer
        std::sync::Arc::ptr_eq(self.value(), &other.0)
    }

    pub(crate) fn into_valid_value(self) -> Result<DiceValidValue, MaybeValidDiceValue> {
        match self {
            Self::Present(v) => Ok(DiceValidValue(v)),
            v @ Self::Transient(_) => Err(v),
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

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceComputedValue {
    value: MaybeValidDiceValue,
    valid: Arc<VersionRanges>,
    invalidation_paths: TrackedInvalidationPaths,
}

#[derive(Allocative, Debug, Clone, Dupe, PartialEq, Eq)]
pub(crate) enum InvalidationPath {
    Clean,
    Unknown,
    Invalidated(Arc<InvalidationPathNode>),
}

impl InvalidationPath {
    fn for_dependent(&self, key: DiceKey) -> InvalidationPath {
        match self {
            InvalidationPath::Clean => InvalidationPath::Clean,
            InvalidationPath::Unknown => InvalidationPath::Unknown,
            InvalidationPath::Invalidated(v) => {
                InvalidationPath::Invalidated(Arc::new(InvalidationPathNode {
                    key,
                    version: v.version,
                    cause: self.dupe(),
                }))
            }
        }
    }

    fn at_version(&self, v: VersionNumber) -> InvalidationPath {
        match self {
            InvalidationPath::Invalidated(t) if t.version > v => InvalidationPath::Unknown,
            _ => self.dupe(),
        }
    }

    fn update(&mut self, other: InvalidationPath) {
        if let InvalidationPath::Invalidated(other) = other {
            match self {
                InvalidationPath::Invalidated(this) if this.version > other.version => {}
                InvalidationPath::Unknown => {}
                InvalidationPath::Clean | InvalidationPath::Invalidated(..) => {
                    *self = InvalidationPath::Invalidated(other);
                }
            }
        }
    }
}

#[derive(Allocative, Debug, Clone, Dupe, PartialEq, Eq)]
pub(crate) struct InvalidationPathNode {
    /// The key at this node in the path.
    pub(crate) key: DiceKey,
    pub(crate) version: VersionNumber,
    pub(crate) cause: InvalidationPath,
}

#[derive(Allocative, Debug, Clone, Dupe, PartialEq, Eq)]
struct InvalidationPathsHeap {
    normal: InvalidationPath,
    high: InvalidationPath,
}

const TAG_ALLOCATED: u8 = 0;
const TAG_BOTH_CLEAN: u8 = 1;
const TAG_BOTH_UNKNOWN: u8 = 2;

#[derive(Allocative, Debug, Clone, Dupe, PartialEq, Eq)]
pub(crate) struct TrackedInvalidationPaths(PackedPtr<Option<Arc<InvalidationPathsHeap>>>);

impl TrackedInvalidationPaths {
    fn from_pair(normal: InvalidationPath, high: InvalidationPath) -> Self {
        match (normal, high) {
            (InvalidationPath::Clean, InvalidationPath::Clean) => {
                Self(PackedPtr::new(None, TAG_BOTH_CLEAN))
            }
            (InvalidationPath::Unknown, InvalidationPath::Unknown) => {
                Self(PackedPtr::new(None, TAG_BOTH_UNKNOWN))
            }
            (normal, high) => Self(PackedPtr::new(
                Some(Arc::new(InvalidationPathsHeap { normal, high })),
                TAG_ALLOCATED,
            )),
        }
    }

    fn get(&self) -> &InvalidationPathsHeap {
        match self.0.extra() {
            TAG_ALLOCATED => self.0.as_ref().unwrap(),
            TAG_BOTH_CLEAN => &InvalidationPathsHeap {
                normal: InvalidationPath::Clean,
                high: InvalidationPath::Clean,
            },
            TAG_BOTH_UNKNOWN => &InvalidationPathsHeap {
                normal: InvalidationPath::Unknown,
                high: InvalidationPath::Unknown,
            },
            _ => unreachable!(),
        }
    }

    pub(crate) fn for_dependent(&self, key: DiceKey) -> TrackedInvalidationPaths {
        let this = self.get();
        TrackedInvalidationPaths::from_pair(
            this.normal.for_dependent(key),
            this.high.for_dependent(key),
        )
    }

    pub(crate) fn clean() -> TrackedInvalidationPaths {
        Self::from_pair(InvalidationPath::Clean, InvalidationPath::Clean)
    }

    pub(crate) fn at_version(&self, v: VersionNumber) -> TrackedInvalidationPaths {
        let this = self.get();
        TrackedInvalidationPaths::from_pair(this.normal.at_version(v), this.high.at_version(v))
    }

    pub(crate) fn new(
        priority: InvalidationSourcePriority,
        key: DiceKey,
        version: VersionNumber,
    ) -> Self {
        let path = InvalidationPath::Invalidated(Arc::new(InvalidationPathNode {
            key,
            version,
            cause: InvalidationPath::Clean,
        }));
        match priority {
            InvalidationSourcePriority::Ignored => Self::clean(),
            InvalidationSourcePriority::Normal => Self::from_pair(path, InvalidationPath::Clean),
            InvalidationSourcePriority::High => Self::from_pair(path.dupe(), path),
        }
    }

    pub(crate) fn update(&mut self, new_paths: TrackedInvalidationPaths) {
        let this = self.get();
        let new_paths = new_paths.get();
        let mut normal = this.normal.dupe();
        let mut high = this.high.dupe();
        normal.update(new_paths.normal.dupe());
        high.update(new_paths.high.dupe());
        *self = Self::from_pair(normal, high);
    }

    pub(crate) fn get_normal(&self) -> InvalidationPath {
        self.get().normal.dupe()
    }

    pub(crate) fn get_high(&self) -> InvalidationPath {
        self.get().high.dupe()
    }
}

impl DiceComputedValue {
    pub(crate) fn new(
        value: MaybeValidDiceValue,
        valid: Arc<VersionRanges>,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> Self {
        Self {
            value,
            valid,
            invalidation_paths,
        }
    }

    pub(crate) fn value(&self) -> &MaybeValidDiceValue {
        &self.value
    }

    pub(crate) fn versions(&self) -> &VersionRanges {
        &self.valid
    }

    pub(crate) fn invalidation_paths(&self) -> &TrackedInvalidationPaths {
        &self.invalidation_paths
    }

    pub(crate) fn into_parts(self) -> (MaybeValidDiceValue, TrackedInvalidationPaths) {
        (self.value, self.invalidation_paths)
    }
}

impl Debug for DiceComputedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceComputedValue")
            .field("valid", &self.valid)
            .finish_non_exhaustive()
    }
}

pub trait DiceValueDyn: Allocative + Any + Send + Sync + 'static {
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
pub mod testing {
    use crate::arc::Arc;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::DiceValueDyn;
    use crate::impls::value::InvalidationPath;
    use crate::impls::value::InvalidationPathNode;
    use crate::impls::value::MaybeValidDiceValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::versions::VersionNumber;

    impl DiceValidValue {
        pub(crate) fn testing_new<V: DiceValueDyn>(value: V) -> Self {
            Self(std::sync::Arc::new(value))
        }
    }

    impl MaybeValidDiceValue {
        pub(crate) fn testing_value(&self) -> &std::sync::Arc<dyn DiceValueDyn> {
            self.value()
        }
    }

    pub struct MakeInvalidationPaths {
        pub normal: (DiceKey, usize),
        pub high: Option<(DiceKey, usize)>,
    }

    impl MakeInvalidationPaths {
        pub fn into(self) -> TrackedInvalidationPaths {
            let normal = InvalidationPath::Invalidated(Arc::new(InvalidationPathNode {
                key: self.normal.0,
                version: VersionNumber::new(self.normal.1),
                cause: InvalidationPath::Clean,
            }));
            let high = self.high.map_or(InvalidationPath::Clean, |(k, v)| {
                InvalidationPath::Invalidated(Arc::new(InvalidationPathNode {
                    key: k,
                    version: VersionNumber::new(v),
                    cause: InvalidationPath::Clean,
                }))
            });
            TrackedInvalidationPaths::from_pair(normal, high)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::impls::value::testing::MakeInvalidationPaths;

    #[test]

    fn test_invalidation_paths() -> anyhow::Result<()> {
        let key0 = DiceKey { index: 0 };
        let key1 = DiceKey { index: 1 };
        let key2 = DiceKey { index: 2 };
        let mut paths = TrackedInvalidationPaths::clean();

        paths.update(
            MakeInvalidationPaths {
                normal: (key0, 2),
                high: None,
            }
            .into(),
        );

        assert_eq!(
            paths,
            MakeInvalidationPaths {
                normal: (key0, 2),
                high: None,
            }
            .into()
        );

        paths.update(
            MakeInvalidationPaths {
                normal: (key1, 3),
                high: Some((key1, 3)),
            }
            .into(),
        );

        assert_eq!(
            paths,
            MakeInvalidationPaths {
                normal: (key1, 3),
                high: Some((key1, 3)),
            }
            .into()
        );

        paths.update(
            MakeInvalidationPaths {
                normal: (key2, 4),
                high: None,
            }
            .into(),
        );

        assert_eq!(
            paths,
            MakeInvalidationPaths {
                normal: (key2, 4),
                high: Some((key1, 3))
            }
            .into()
        );

        Ok(())
    }
}
