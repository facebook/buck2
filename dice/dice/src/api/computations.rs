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
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::*;
use more_futures::spawn::spawn_dropcancel;

use crate::api::data::DiceData;
use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::opaque::OpaqueValue;
use crate::api::transaction::DiceTransaction;
use crate::api::user_data::UserComputationData;
use crate::ctx::DiceComputationImpl;
use crate::opaque::OpaqueValueImpl;

/// The context for computations to register themselves, and request for additional dependencies.
/// The dependencies accessed are tracked for caching via the `DiceCtx`.
///
/// The computations are registered onto `DiceComputations` via implementing traits for the
/// `DiceComputation`.
///
/// The context is valid only for the duration of the computation of a single key, and cannot be
/// owned.
#[derive(Allocative)]
pub struct DiceComputations(pub(crate) Arc<DiceComputationImpl>);

fn _test_computations_sync_send() {
    fn _assert_sync_send<T: Sync + Send>() {}
    _assert_sync_send::<DiceComputations>();
}

impl DiceComputations {
    /// Gets all the result of of the given computation key.
    /// recorded as dependencies of the current computation for which this
    /// context is for.
    pub fn compute<'a, K>(
        &'a self,
        key: &'a K,
    ) -> impl Future<Output = DiceResult<<K as Key>::Value>> + 'a
    where
        K: Key,
    {
        self.0
            .compute_opaque(key)
            .map(|r| r.map(OpaqueValueImpl::into_value))
    }

    /// same as `compute` but for a multiple keys. The returned results will be in order of the
    /// keys given
    pub fn compute_many<'a, K>(
        &'a self,
        keys: &[&'a K],
    ) -> Vec<impl Future<Output = DiceResult<<K as Key>::Value>> + 'a>
    where
        K: Key,
    {
        keys.map(|k| self.compute(*k))
    }

    /// Compute "opaque" value where the value is only accessible via projections.
    /// Projections allow accessing derived results from the "opaque" value,
    /// where the dependency of reading a projection is the projection value rather
    /// than the entire opaque value.
    pub fn compute_opaque<'b, 'a: 'b, K>(
        &'a self,
        key: &'b K,
    ) -> impl Future<Output = DiceResult<OpaqueValue<'a, K>>> + 'b
    where
        K: Key,
    {
        self.0.compute_opaque(key).map(|r| r.map(OpaqueValue::new))
    }

    /// temporarily here while we figure out why dice isn't paralleling computations so that we can
    /// use this in tokio spawn. otherwise, this shouldn't be here so that we don't need to clone
    /// the Arc, which makes lifetimes weird.
    pub fn temporary_spawn<F, FUT, R>(&self, f: F) -> impl Future<Output = R> + Send + 'static
    where
        F: FnOnce(DiceTransaction) -> FUT + Send + 'static,
        FUT: Future<Output = R> + Send,
        R: Send + 'static,
    {
        // Don't make this function async. It should perform the spawn without needing to poll the returned future.
        let duped = DiceTransaction(DiceComputations(self.0.dupe()));

        spawn_dropcancel(
            async move { f(duped).await },
            self.0.extra.user_data.spawner.as_ref(),
            &self.0.extra.user_data,
            debug_span!(parent: None, "spawned_task",),
        )
    }

    /// Data that is static per the entire lifetime of Dice. These data are initialized at the
    /// time that Dice is initialized via the constructor.
    pub fn global_data(&self) -> &DiceData {
        &self.0.dice.data
    }

    /// Data that is static for the lifetime of the current request context. This lifetime is
    /// the lifetime of the top-level `DiceComputation` used for all requests.
    /// The data is also specific to each request context, so multiple concurrent requests can
    /// each have their own individual data.
    pub fn per_transaction_data(&self) -> &UserComputationData {
        &self.0.extra.user_data
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::api::computations::DiceComputations;
    use crate::incremental::versions::MinorVersion;
    use crate::incremental::versions::VersionNumber;

    pub(crate) trait DiceCtxExt {
        fn get_version(&self) -> VersionNumber;
        fn get_minor_version(&self) -> MinorVersion;
    }

    impl DiceCtxExt for DiceComputations {
        fn get_version(&self) -> VersionNumber {
            self.0.transaction_ctx.get_version()
        }

        fn get_minor_version(&self) -> MinorVersion {
            self.0.transaction_ctx.get_minor_version()
        }
    }
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
    use crate::api::user_data::UserComputationData;
    use crate::ctx::ComputationData;
    use crate::cycles::RequestedKey;

    #[derive(Clone, Dupe, Display, Debug, PartialEq, Eq, Hash, Allocative)]
    struct K(usize);

    #[test]
    fn cycle_detection_when_no_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest(&K(1))?;
        let ctx = ctx.subrequest(&K(2))?;
        let ctx = ctx.subrequest(&K(3))?;
        let _ctx = ctx.subrequest(&K(4))?;

        Ok(())
    }

    #[test]
    fn cycle_detection_when_cycles() -> anyhow::Result<()> {
        let ctx = ComputationData::new(UserComputationData::new(), DetectCycles::Enabled);
        let ctx = ctx.subrequest(&K(1))?;
        let ctx = ctx.subrequest(&K(2))?;
        let ctx = ctx.subrequest(&K(3))?;
        let ctx = ctx.subrequest(&K(4))?;
        match ctx.subrequest(&K(1)) {
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
