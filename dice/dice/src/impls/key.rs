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
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use dupe::Dupe;
use fnv::FnvHasher;
use gazebo::cmp::PartialEqAny;

use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::api::projection::DiceProjectionComputations;
use crate::api::projection::ProjectionKey;
use crate::impls::hash;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceProjectValue;
use crate::impls::value::DiceValue;

/// Type erased internal dice key
#[derive(
    Allocative, Eq, PartialEq, Clone, Copy, Dupe, Hash, Debug, Ord, PartialOrd
)]
pub(crate) struct DiceKey {
    /// represented as indexes into an internal index based map
    pub(crate) index: u32,
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) enum DiceKeyErased {
    Key(Arc<dyn DiceKeyDyn>),
    Projection(ProjectionWithBase),
}

impl DiceKeyErased {
    pub(crate) fn key<K: Key>(k: K) -> Self {
        Self::Key(Arc::new(k))
    }

    #[allow(unused)]
    pub(crate) fn proj<K: ProjectionKey>(base: DiceKey, k: K) -> Self {
        Self::Projection(ProjectionWithBase {
            base,
            proj: Arc::new(k),
        })
    }

    #[allow(unused)] // TODO(bobyf) temporary
    pub(crate) fn key_type_name(&self) -> &'static str {
        match self {
            DiceKeyErased::Key(k) => k.key_type_name(),
            DiceKeyErased::Projection(k) => k.proj.key_type_name(),
        }
    }

    pub(crate) fn hash(&self) -> u64 {
        match self {
            DiceKeyErased::Key(k) => k.hash(),
            DiceKeyErased::Projection(proj) => proj.hash(),
        }
    }

    pub(crate) fn downcast<K: 'static>(self) -> Option<Arc<K>> {
        match self {
            DiceKeyErased::Key(k) => {
                if k.as_any().is::<K>() {
                    Some(unsafe { Arc::from_raw(Arc::into_raw(k).cast()) })
                } else {
                    None
                }
            }
            DiceKeyErased::Projection(proj) => {
                if proj.proj.as_any().is::<K>() {
                    Some(unsafe { Arc::from_raw(Arc::into_raw(proj.proj).cast()) })
                } else {
                    None
                }
            }
        }
    }

    pub(crate) fn as_ref<'a>(&'a self) -> DiceKeyErasedRef<'a> {
        match self {
            DiceKeyErased::Key(k) => DiceKeyErasedRef::Key(&**k),
            DiceKeyErased::Projection(proj) => {
                DiceKeyErasedRef::Projection(ProjectionWithBaseRef {
                    base: proj.base,
                    proj: &*proj.proj,
                })
            }
        }
    }
}

impl PartialEq for DiceKeyErased {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DiceKeyErased::Key(k), DiceKeyErased::Key(ok)) => k.eq_any() == ok.eq_any(),
            (DiceKeyErased::Projection(k), DiceKeyErased::Projection(ok)) => k == ok,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Dupe)]
pub(crate) enum DiceKeyErasedRef<'a> {
    Key(&'a dyn DiceKeyDyn),
    Projection(ProjectionWithBaseRef<'a>),
}

impl<'a> DiceKeyErasedRef<'a> {
    pub(crate) fn key<K: Key>(k: &'a K) -> Self {
        Self::Key(k)
    }

    #[allow(unused)] // TODO(bobyf)
    pub(crate) fn proj<K: ProjectionKey>(base: DiceKey, k: &'a K) -> Self {
        Self::Projection(ProjectionWithBaseRef { base, proj: k })
    }

    pub(crate) fn hash(&self) -> u64 {
        match self {
            DiceKeyErasedRef::Key(k) => k.hash(),
            DiceKeyErasedRef::Projection(p) => p.hash(),
        }
    }

    #[allow(unused)] // TODO(bobyf) temporary
    pub(crate) fn key_type_name(&self) -> &'static str {
        match self {
            DiceKeyErasedRef::Key(k) => k.key_type_name(),
            DiceKeyErasedRef::Projection(k) => k.proj.key_type_name(),
        }
    }

    fn to_owned(&self) -> DiceKeyErased {
        match self {
            DiceKeyErasedRef::Key(k) => DiceKeyErased::Key(k.clone_arc()),
            DiceKeyErasedRef::Projection(p) => DiceKeyErased::Projection(p.to_owned()),
        }
    }
}

impl<'a> PartialEq for DiceKeyErasedRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (DiceKeyErasedRef::Key(k), DiceKeyErasedRef::Key(ok)) => k.eq_any() == ok.eq_any(),
            (DiceKeyErasedRef::Projection(k), DiceKeyErasedRef::Projection(ok)) => k == ok,
            _ => false,
        }
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
            CowDiceKey::Ref(r) => r.to_owned(),
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

    fn key_type_name(&self) -> &'static str;
}

#[async_trait]
impl<K> DiceKeyDyn for K
where
    K: Key,
{
    async fn compute(&self, ctx: &DiceComputations) -> DiceValue {
        let value = self.compute(ctx).await;
        DiceValue::new(DiceKeyValue::<K>::new(value))
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

    fn key_type_name(&self) -> &'static str {
        K::key_type_name()
    }
}

pub(crate) trait DiceProjectionDyn: Allocative + Send + Sync + 'static {
    fn compute(&self, derive_from: DiceValue, ctx: &DiceProjectionComputations) -> DiceValue;

    fn eq_any(&self) -> PartialEqAny;

    fn hash(&self) -> u64;

    fn as_any(&self) -> &dyn Any;

    fn clone_arc(&self) -> Arc<dyn DiceProjectionDyn>;

    fn key_type_name(&self) -> &'static str;
}

impl<K> DiceProjectionDyn for K
where
    K: ProjectionKey,
{
    fn compute(&self, derive_from: DiceValue, ctx: &DiceProjectionComputations) -> DiceValue {
        let value = self.compute(
            derive_from
                .downcast_ref()
                .expect("Projection Key derived from wrong type"),
            ctx,
        );
        DiceValue::new(DiceProjectValue::<K>::new(value))
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

    fn clone_arc(&self) -> Arc<dyn DiceProjectionDyn> {
        Arc::new(self.clone())
    }

    fn key_type_name(&self) -> &'static str {
        K::key_type_name()
    }
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct ProjectionWithBase {
    base: DiceKey,
    proj: Arc<dyn DiceProjectionDyn>,
}

impl ProjectionWithBase {
    fn hash(&self) -> u64 {
        let mut hasher = FnvHasher::default();

        self.base.hash(&mut hasher);
        self.proj.hash().hash(&mut hasher);

        hasher.finish()
    }
}

impl PartialEq for ProjectionWithBase {
    fn eq(&self, other: &Self) -> bool {
        self.proj.eq_any() == other.proj.eq_any() && self.base == other.base
    }
}

impl Eq for ProjectionWithBase {}

#[derive(Copy, Clone, Dupe)]
pub(crate) struct ProjectionWithBaseRef<'a> {
    base: DiceKey,
    proj: &'a dyn DiceProjectionDyn,
}

impl<'a> ProjectionWithBaseRef<'a> {
    fn to_owned(&self) -> ProjectionWithBase {
        ProjectionWithBase {
            base: self.base,
            proj: self.proj.clone_arc(),
        }
    }

    fn hash(&self) -> u64 {
        let mut hasher = FnvHasher::default();

        self.base.hash(&mut hasher);
        self.proj.hash().hash(&mut hasher);

        hasher.finish()
    }
}

impl<'a> PartialEq for ProjectionWithBaseRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.proj.eq_any() == other.proj.eq_any() && self.base == other.base
    }
}

impl<'a> Eq for ProjectionWithBaseRef<'a> {}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::api::projection::DiceProjectionComputations;
    use crate::api::projection::ProjectionKey;
    use crate::impls::key::DiceKey;
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

        let erased = DiceKeyErased::key(TestK);

        let downcast = erased.downcast::<TestK>();
        assert!(downcast.is_some());
        let downcast = downcast.unwrap();

        assert_eq!(&*downcast, &TestK);

        // no extra copies
        assert_eq!(Arc::strong_count(&downcast), 1);

        #[derive(Allocative, Debug, Display, Clone, Dupe, Eq, PartialEq, Hash)]
        struct TestProj;

        impl ProjectionKey for TestProj {
            type DeriveFromKey = TestK;
            type Value = ();

            fn compute(
                &self,
                _derive_from: &<<Self as ProjectionKey>::DeriveFromKey as Key>::Value,
                _ctx: &DiceProjectionComputations,
            ) -> Self::Value {
                unimplemented!("test")
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                unimplemented!("test")
            }
        }

        let erased = DiceKeyErased::proj(DiceKey { index: 99 }, TestProj);

        let downcast = erased.downcast::<TestProj>();
        assert!(downcast.is_some());
        let downcast = downcast.unwrap();

        assert_eq!(&*downcast, &TestProj);

        // no extra copies
        assert_eq!(Arc::strong_count(&downcast), 1);
    }
}
