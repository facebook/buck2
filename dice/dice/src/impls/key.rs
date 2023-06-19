/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use fnv::FnvHasher;
use gazebo::cmp::PartialEqAny;
use more_futures::cancellation::CancellationContext;

use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::api::projection::DiceProjectionComputations;
use crate::api::projection::ProjectionKey;
use crate::api::storage_type::StorageType;
use crate::impls::hash;
use crate::impls::hash::key_hash;
use crate::impls::value::DiceKeyValue;
use crate::impls::value::DiceProjectValue;
use crate::impls::value::DiceValueDyn;
use crate::impls::value::MaybeValidDiceValue;

/// Type erased internal dice key
#[derive(
    Allocative, Eq, PartialEq, Clone, Copy, Dupe, Hash, Debug, Ord, PartialOrd
)]
pub(crate) struct DiceKey {
    /// represented as indexes into an internal index based map
    pub(crate) index: u32,
}

/// Key of the parent computation
#[derive(
    Allocative, Eq, PartialEq, Clone, Copy, Dupe, Hash, Debug, Ord, PartialOrd
)]
pub(crate) enum ParentKey {
    None,
    Some(DiceKey),
}

#[derive(Allocative, Clone, Dupe, Display)]
pub(crate) enum DiceKeyErased {
    Key(Arc<dyn DiceKeyDyn>),
    Projection(ProjectionWithBase),
}

impl DiceKeyErased {
    pub(crate) fn key<K: Key>(k: K) -> Self {
        Self::Key(Arc::new(k))
    }

    #[allow(unused)] // counterpart of projection to `key`
    pub(crate) fn proj<K: ProjectionKey>(base: DiceKey, k: K) -> Self {
        Self::Projection(ProjectionWithBase {
            base,
            proj: Arc::new(k),
        })
    }

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

    pub(crate) fn as_any(&self) -> &dyn Any {
        match self {
            DiceKeyErased::Key(k) => k.as_any(),
            DiceKeyErased::Projection(proj) => proj.proj().as_any(),
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

impl Eq for DiceKeyErased {}

#[derive(Copy, Clone, Dupe, Display)]
pub(crate) enum DiceKeyErasedRef<'a> {
    Key(&'a dyn DiceKeyDyn),
    Projection(ProjectionWithBaseRef<'a>),
}

impl<'a> DiceKeyErasedRef<'a> {
    pub(crate) fn key<K: Key>(k: &'a K) -> Self {
        Self::Key(k)
    }

    pub(crate) fn proj<K: ProjectionKey>(base: DiceKey, k: &'a K) -> Self {
        Self::Projection(ProjectionWithBaseRef { base, proj: k })
    }

    pub(crate) fn hash(&self) -> u64 {
        match self {
            DiceKeyErasedRef::Key(k) => k.hash(),
            DiceKeyErasedRef::Projection(p) => p.hash(),
        }
    }

    #[allow(unused)] // generally useful function that is the counterpart on `DiceKeyErased` 
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

#[derive(Display)]
pub(crate) enum CowDiceKey<'a> {
    Ref(DiceKeyErasedRef<'a>),
    Owned(DiceKeyErased),
}

impl<'a> CowDiceKey<'a> {
    pub(crate) fn into_owned(self) -> DiceKeyErased {
        match self {
            CowDiceKey::Ref(r) => r.to_owned(),
            CowDiceKey::Owned(owned) => owned,
        }
    }

    pub(crate) fn borrow(&'a self) -> DiceKeyErasedRef<'a> {
        match self {
            CowDiceKey::Ref(r) => *r,
            CowDiceKey::Owned(owned) => owned.as_ref(),
        }
    }
}

pub(crate) struct CowDiceKeyHashed<'a> {
    cow: CowDiceKey<'a>,
    hash: u64,
}

impl<'a> CowDiceKeyHashed<'a> {
    pub(crate) fn key_ref<K: Key>(key: &K) -> CowDiceKeyHashed {
        let hash = key_hash(key);
        CowDiceKeyHashed {
            cow: CowDiceKey::Ref(DiceKeyErasedRef::key(key)),
            hash,
        }
    }

    pub(crate) fn proj_ref<K: ProjectionKey>(base: DiceKey, k: &'a K) -> CowDiceKeyHashed {
        let key = DiceKeyErasedRef::proj(base, k);
        let hash = key.hash();
        CowDiceKeyHashed {
            cow: CowDiceKey::Ref(key),
            hash,
        }
    }

    pub(crate) fn key<K: Key>(key: K) -> CowDiceKeyHashed<'static> {
        let hash = key_hash(&key);
        CowDiceKeyHashed {
            cow: CowDiceKey::Owned(DiceKeyErased::key(key)),
            hash,
        }
    }

    pub(crate) fn hash(&self) -> u64 {
        self.hash
    }

    pub(crate) fn into_cow(self) -> CowDiceKey<'a> {
        self.cow
    }
}

impl<'a> Display for CowDiceKeyHashed<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.cow)
    }
}

#[async_trait]
pub(crate) trait DiceKeyDyn: Allocative + Display + Send + Sync + 'static {
    async fn compute(
        &self,
        ctx: &DiceComputations,
        cancellations: &CancellationContext,
    ) -> Arc<dyn DiceValueDyn>;

    fn eq_any(&self) -> PartialEqAny;

    fn hash(&self) -> u64;

    fn as_any(&self) -> &dyn Any;

    fn clone_arc(&self) -> Arc<dyn DiceKeyDyn>;

    fn key_type_name(&self) -> &'static str;

    fn storage_type(&self) -> StorageType;
}

#[async_trait]
impl<K> DiceKeyDyn for K
where
    K: Key,
{
    async fn compute(
        &self,
        ctx: &DiceComputations,
        cancellations: &CancellationContext,
    ) -> Arc<dyn DiceValueDyn> {
        let value = self.compute(ctx, cancellations).await;
        Arc::new(DiceKeyValue::<K>::new(value))
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

    fn storage_type(&self) -> StorageType {
        K::storage_type()
    }
}

pub(crate) trait DiceProjectionDyn: Allocative + Display + Send + Sync + 'static {
    fn compute(
        &self,
        derive_from: &MaybeValidDiceValue,
        ctx: &DiceProjectionComputations,
    ) -> Arc<dyn DiceValueDyn>;

    fn eq_any(&self) -> PartialEqAny;

    fn hash(&self) -> u64;

    fn as_any(&self) -> &dyn Any;

    fn clone_arc(&self) -> Arc<dyn DiceProjectionDyn>;

    fn key_type_name(&self) -> &'static str;

    fn storage_type(&self) -> StorageType;
}

impl<K> DiceProjectionDyn for K
where
    K: ProjectionKey,
{
    fn compute(
        &self,
        derive_from: &MaybeValidDiceValue,
        ctx: &DiceProjectionComputations,
    ) -> Arc<dyn DiceValueDyn> {
        let value = self.compute(
            derive_from
                .downcast_maybe_transient()
                .expect("Projection Key derived from wrong type"),
            ctx,
        );
        Arc::new(DiceProjectValue::<K>::new(value))
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

    fn storage_type(&self) -> StorageType {
        K::storage_type()
    }
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct ProjectionWithBase {
    base: DiceKey,
    proj: Arc<dyn DiceProjectionDyn>,
}

impl ProjectionWithBase {
    pub(crate) fn base(&self) -> DiceKey {
        self.base
    }

    pub(crate) fn proj(&self) -> &dyn DiceProjectionDyn {
        &*self.proj
    }

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

impl Display for ProjectionWithBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.proj)
    }
}

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

impl<'a> Display for ProjectionWithBaseRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.proj)
    }
}

mod introspection {
    use std::fmt::Display;
    use std::fmt::Formatter;
    use std::hash::Hash;
    use std::hash::Hasher;

    use dupe::Dupe;
    use gazebo::cmp::PartialEqAny;

    use crate::impls::key::DiceKey;
    use crate::impls::key::DiceKeyErased;
    use crate::introspection::graph::AnyKey;
    use crate::introspection::graph::KeyForIntrospection;
    use crate::introspection::graph::KeyID;

    impl DiceKey {
        pub(crate) fn introspect(&self) -> KeyID {
            KeyID(self.index as usize)
        }
    }

    impl DiceKeyErased {
        pub(crate) fn introspect(&self) -> AnyKey {
            #[derive(Clone, Dupe)]
            struct Wrap(DiceKeyErased);

            impl PartialEq for Wrap {
                fn eq(&self, other: &Self) -> bool {
                    self.0 == other.0
                }
            }

            impl Eq for Wrap {}

            impl Display for Wrap {
                fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
                    match &self.0 {
                        DiceKeyErased::Key(k) => {
                            write!(f, "{}", k)
                        }
                        DiceKeyErased::Projection(p) => {
                            write!(f, "{}", p.proj)
                        }
                    }
                }
            }

            impl KeyForIntrospection for Wrap {
                fn get_key_equality(&self) -> PartialEqAny {
                    PartialEqAny::new(self)
                }

                fn hash(&self, mut state: &mut dyn Hasher) {
                    Hash::hash(&self.0.hash(), &mut state);
                }

                fn box_clone(&self) -> Box<dyn KeyForIntrospection> {
                    Box::new(self.clone())
                }

                fn type_name(&self) -> &'static str {
                    match &self.0 {
                        DiceKeyErased::Key(k) => k.key_type_name(),
                        DiceKeyErased::Projection(p) => p.proj.key_type_name(),
                    }
                }
            }

            AnyKey::new(Wrap(self.dupe()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;
    use more_futures::cancellation::CancellationContext;

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

            async fn compute(
                &self,
                _ctx: &DiceComputations,
                _cancellations: &CancellationContext,
            ) -> Self::Value {
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
