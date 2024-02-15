/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use futures::future::BoxFuture;

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationsImpl;
use crate::ProjectionKey;
use crate::UserCycleDetectorGuard;

/// The context for computations to register themselves, and request for additional dependencies.
/// The dependencies accessed are tracked for caching via the `DiceCtx`.
///
/// The computations are registered onto `DiceComputations` via implementing traits for the
/// `DiceComputation`.
///
/// The context is valid only for the duration of the computation of a single key, and cannot be
/// owned.
#[derive(Allocative)]
pub struct DiceComputations<'a>(DiceComputationsKind<'a>);

// separate because we don't want the cases to be pub.
#[derive(Allocative)]
enum DiceComputationsKind<'a> {
    // Used by the initial DiceComputations that a key sees (or when created for a transaction).
    Owned(DiceComputationsImpl),

    // Used for cases where we create a "sub-computations" for the initial computation, like in compute_many or compute2.
    #[allocative(skip)]
    Borrowed(&'a DiceComputationsImpl),
}

fn _test_computations_sync_send() {
    fn _assert_sync_send<T: Sync + Send>() {}
    _assert_sync_send::<DiceComputations>();
}

impl<'d> DiceComputations<'d> {
    pub(crate) fn borrowed(inner: &'d DiceComputationsImpl) -> Self {
        DiceComputations(DiceComputationsKind::Borrowed(inner))
    }

    pub(crate) fn new(inner: DiceComputationsImpl) -> Self {
        DiceComputations(DiceComputationsKind::Owned(inner))
    }

    /// To support migration to a &mut-based DiceComputations api, this allows callers to get an
    /// owned DiceComputations (which effectively allows upgrading a &DiceComputations to a &mut
    /// DiceComputations). That makes it easier to incrementally migrate things to the &mut api.
    pub fn bad_dice<'a>(&'a self) -> DiceComputations<'a> {
        DiceComputations::borrowed(self.inner())
    }

    pub(crate) fn inner(&self) -> &DiceComputationsImpl {
        match &self.0 {
            DiceComputationsKind::Owned(v) => &v,
            DiceComputationsKind::Borrowed(v) => v,
        }
    }

    pub(crate) fn try_into_inner(self) -> Option<DiceComputationsImpl> {
        match self.0 {
            DiceComputationsKind::Owned(v) => Some(v),
            DiceComputationsKind::Borrowed(_) => None,
        }
    }

    /// Gets the result of the given computation key.
    /// Record dependencies of the current computation for which this
    /// context is for.
    pub fn compute<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        self.inner().compute(key)
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub fn compute_opaque<'a, K>(
        &'a self,
        key: &K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<K>>> + 'a
    where
        K: Key,
    {
        self.inner().compute_opaque(key)
    }

    pub fn projection<'a, K: Key, P: ProjectionKey<DeriveFromKey = K>>(
        &'a self,
        derive_from: &OpaqueValue<K>,
        projection_key: &P,
    ) -> DiceResult<P::Value> {
        self.inner().projection(derive_from, projection_key)
    }

    pub fn opaque_into_value<'a, K: Key>(
        &'a self,
        derive_from: OpaqueValue<K>,
    ) -> DiceResult<K::Value> {
        self.inner().opaque_into_value(derive_from)
    }

    /// Computes all the given tasks in parallel, returning an unordered Stream.
    ///
    /// ```ignore
    /// let ctx: &'a DiceComputations = ctx();
    /// let data: String = data();
    /// let keys : Vec<Key> = keys();
    /// ctx.compute_many(keys.into_iter().map(|k|
    ///   higher_order_closure! {
    ///     #![with<'a>]
    ///     for <'x> move |dice: &'x mut DiceComputations<'a>| -> BoxFuture<'x, String> {
    ///       async move {
    ///         dice.compute(k).await + data
    ///       }.boxed()
    ///     }
    ///   }
    /// )).await;
    /// ```
    /// The `#![with<'a>]` is required for the closure to capture any non-'static references.
    pub fn compute_many<'a, T: 'a>(
        &'a self,
        computes: impl IntoIterator<
            Item = impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        >,
    ) -> Vec<impl Future<Output = T> + 'a> {
        self.inner().compute_many(computes)
    }

    /// Computes all the given tasks in parallel, returning an unordered Stream
    pub fn compute2<'a, T: 'a, U: 'a>(
        &'a self,
        compute1: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, T> + Send,
        compute2: impl for<'x> FnOnce(&'x mut DiceComputations<'a>) -> BoxFuture<'x, U> + Send,
    ) -> (impl Future<Output = T> + 'a, impl Future<Output = U> + 'a) {
        self.inner().compute2(compute1, compute2)
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub fn global_data(&self) -> &DiceData {
        self.inner().global_data()
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub fn per_transaction_data(&self) -> &UserComputationData {
        self.inner().per_transaction_data()
    }

    /// Gets the current cycle guard if its set. If it's set but a different type, an error will be returned.
    pub fn cycle_guard<T: UserCycleDetectorGuard>(&self) -> DiceResult<Option<&T>> {
        self.inner().cycle_guard()
    }

    /// Store some extra data that the ActivationTracker will receive if / when this key finishes
    /// executing.
    pub fn store_evaluation_data<T: Send + Sync + 'static>(&self, value: T) -> DiceResult<()> {
        self.inner().store_evaluation_data(value)
    }
}

// This assertion assures we don't unknowingly regress the size of this critical future.
// TODO(cjhopman): We should be able to wrap this in a convenient assertion macro.
#[allow(unused, clippy::diverging_sub_expression)]
fn _assert_dice_compute_future_sizes() {
    let ctx: DiceComputations = panic!();
    #[derive(Allocative, Debug, Clone, PartialEq, Eq, Hash)]
    struct K(u64);
    impl std::fmt::Display for K {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            panic!()
        }
    }
    #[async_trait]
    impl Key for K {
        type Value = Arc<String>;

        async fn compute(
            &self,
            ctx: &mut DiceComputations,
            cancellations: &CancellationContext,
        ) -> Self::Value {
            panic!()
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            panic!()
        }
    }
    let k: K = panic!();
    let v = ctx.compute(&k);
    let e = [0u8; 640 / 8];
    static_assertions::assert_eq_size_ptr!(&v, &e);
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use derive_more::Display;
    use dupe::Dupe;
    use indexmap::indexset;

    use crate::api::cycles::DetectCycles;
    use crate::api::error::DiceErrorImpl;
    use crate::api::storage_type::StorageType;
    use crate::api::user_data::UserComputationData;
    use crate::legacy::ctx::ComputationData;
    use crate::legacy::cycles::RequestedKey;
    use crate::legacy::incremental::graph::storage_properties::StorageProperties;

    #[derive(Clone, Dupe, Display, Debug, PartialEq, Eq, Hash, Allocative)]
    struct K(usize);
    impl StorageProperties for K {
        type Key = K;

        type Value = ();

        fn key_type_name() -> &'static str {
            unreachable!()
        }

        fn to_key_any(key: &Self::Key) -> &dyn std::any::Any {
            key
        }

        fn storage_type(&self) -> StorageType {
            unreachable!()
        }

        fn equality(&self, _x: &Self::Value, _y: &Self::Value) -> bool {
            unreachable!()
        }

        fn validity(&self, _x: &Self::Value) -> bool {
            unreachable!()
        }
    }

    #[test]
    fn cycle_detection_when_no_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest::<K>(&K(1))?;
        let ctx = ctx.subrequest::<K>(&K(2))?;
        let ctx = ctx.subrequest::<K>(&K(3))?;
        let _ctx = ctx.subrequest::<K>(&K(4))?;

        Ok(())
    }

    #[test]
    fn cycle_detection_when_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest::<K>(&K(1))?;
        let ctx = ctx.subrequest::<K>(&K(2))?;
        let ctx = ctx.subrequest::<K>(&K(3))?;
        let ctx = ctx.subrequest::<K>(&K(4))?;
        match ctx.subrequest::<K>(&K(1)) {
            Ok(_) => {
                panic!("should have cycle error")
            }
            Err(e) => match &*e.0 {
                DiceErrorImpl::Cycle {
                    trigger,
                    cyclic_keys,
                } => {
                    assert!(
                        (**trigger).get_key_equality() == K(1).get_key_equality(),
                        "expected trigger key to be `{}` but was `{}`",
                        K(1),
                        trigger
                    );
                    assert_eq!(
                        cyclic_keys,
                        &indexset![
                            Arc::new(K(1)) as Arc<dyn RequestedKey>,
                            Arc::new(K(2)) as Arc<dyn RequestedKey>,
                            Arc::new(K(3)) as Arc<dyn RequestedKey>,
                            Arc::new(K(4)) as Arc<dyn RequestedKey>
                        ]
                    )
                }
                _ => {
                    panic!("wrong error type")
                }
            },
        }

        Ok(())
    }
}
