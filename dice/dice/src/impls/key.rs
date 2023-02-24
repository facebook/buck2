/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;

use crate::api::key::Key;
use crate::impls::hash;
use crate::impls::value::DiceComputedValue;
use crate::impls::value::DiceValue;
use crate::DiceComputations;

/// Type erased internal dice key
#[derive(
    Allocative, Eq, PartialEq, Clone, Copy, Dupe, Hash, Debug, Ord, PartialOrd
)]
pub(crate) struct DiceKey {
    /// represented as indexes into an internal index based map
    pub(crate) index: u32,
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct DiceKeyErased(Arc<dyn DiceKeyDyn>);

impl DiceKeyErased {
    pub(crate) fn new<K: Key>(k: K) -> Self {
        Self(Arc::new(k))
    }

    pub(crate) fn hash(&self) -> u64 {
        self.0.hash()
    }

    pub(crate) fn eq_any(&self) -> PartialEqAny {
        self.0.eq_any()
    }

    pub(crate) fn downcast<K: Key>(self) -> Option<Arc<K>> {
        if self.0.as_any().is::<K>() {
            Some(unsafe { Arc::from_raw(Arc::into_raw(self.0).cast()) })
        } else {
            None
        }
    }

    fn as_ref(&self) -> DiceKeyErasedRef {
        DiceKeyErasedRef(&*self.0)
    }
}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct DiceKeyErasedRef<'a>(&'a dyn DiceKeyDyn);

impl<'a> DiceKeyErasedRef<'a> {
    pub(crate) fn new<K: Key>(k: &'a K) -> Self {
        Self(k)
    }

    pub(crate) fn hash(&self) -> u64 {
        self.0.hash()
    }

    pub(crate) fn eq_any(&self) -> PartialEqAny {
        self.0.eq_any()
    }
}

pub(crate) enum CowDiceKey<'a> {
    #[allow(unused)]
    Borrow(&'a DiceKeyErased),
    Ref(DiceKeyErasedRef<'a>),
    Owned(DiceKeyErased),
}

impl<'a> CowDiceKey<'a> {
    pub(crate) fn into_owned(self) -> DiceKeyErased {
        match self {
            CowDiceKey::Borrow(b) => b.dupe(),
            CowDiceKey::Ref(r) => DiceKeyErased(r.0.clone_arc()),
            CowDiceKey::Owned(owned) => owned,
        }
    }

    pub(crate) fn borrow(&'a self) -> DiceKeyErasedRef<'a> {
        match self {
            CowDiceKey::Borrow(b) => b.as_ref(),
            CowDiceKey::Ref(r) => *r,
            CowDiceKey::Owned(owned) => owned.as_ref(),
        }
    }
}

#[async_trait]
pub(crate) trait DiceKeyDyn: Allocative + Send + Sync + 'static {
    async fn compute(&self, ctx: &DiceComputations) -> DiceValue;

    fn eq_any(&self) -> PartialEqAny;

    fn hash(&self) -> u64;

    fn as_any(&self) -> &dyn Any;

    fn clone_arc(&self) -> Arc<dyn DiceKeyDyn>;
}

#[async_trait]
impl<K> DiceKeyDyn for K
where
    K: Key,
{
    async fn compute(&self, ctx: &DiceComputations) -> DiceValue {
        let value = self.compute(ctx).await;
        DiceValue::new(DiceComputedValue::<K>::new(value))
    }

    fn eq_any(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self) -> u64 {
        hash::key_hash(self)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn clone_arc(&self) -> Arc<dyn DiceKeyDyn> {
        Arc::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::impls::key::DiceKeyErased;

    #[test]
    fn downcast_key_does_not_increase_refs() {
        #[derive(Allocative, Debug, Display, Clone, Dupe, Eq, PartialEq, Hash)]
        struct TestK;

        #[async_trait::async_trait]
        impl Key for TestK {
            type Value = ();

            async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
                unimplemented!("test")
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                unimplemented!("test")
            }
        }

        let erased = DiceKeyErased::new(TestK);

        let downcast = erased.downcast::<TestK>();
        assert!(downcast.is_some());
        let downcast = downcast.unwrap();

        assert_eq!(&*downcast, &TestK);

        // no extra copies
        assert_eq!(Arc::strong_count(&downcast), 1);
    }
}
