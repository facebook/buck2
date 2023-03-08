/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derivative::Derivative;
use dupe::Dupe;

use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::impls::ctx::PerComputeCtx;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceValidity;

#[derive(Derivative)]
#[derivative(Debug)]
pub(crate) struct OpaqueValueModern<'a, K: Key> {
    derive_from_key: DiceKey,
    #[derivative(Debug = "ignore")]
    derive_from: K::Value,
    #[derivative(Debug = "ignore")]
    parent_computation: &'a PerComputeCtx,
}

impl<'a, K> OpaqueValueModern<'a, K>
where
    K: Key,
{
    pub(crate) fn new(
        parent_computation: &'a PerComputeCtx,
        derive_from_key: DiceKey,
        derive_from: K::Value,
    ) -> Self {
        Self {
            derive_from_key,
            derive_from,
            parent_computation,
        }
    }

    pub(crate) fn projection<P>(&self, projection_key: &P) -> DiceResult<P::Value>
    where
        P: ProjectionKey<DeriveFromKey = K>,
    {
        self.parent_computation.project(
            projection_key,
            self.derive_from_key,
            self.derive_from.dupe(),
        )
    }

    /// Get a value and record parent computation dependency on `K`.
    pub(crate) fn into_value(self) -> K::Value {
        self.parent_computation.dep_trackers().record(
            self.derive_from_key,
            if K::validity(&self.derive_from) {
                DiceValidity::Valid
            } else {
                DiceValidity::Transient
            },
        );

        self.derive_from
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;

    use crate::api::data::DiceData;
    use crate::api::key::Key;
    use crate::impls::dep_trackers::testing::RecordingDepsTrackersExt;
    use crate::impls::dice::DiceModern;
    use crate::impls::key::DiceKey;
    use crate::impls::opaque::OpaqueValueModern;
    use crate::DiceComputations;
    use crate::HashSet;

    #[derive(Allocative, Clone, Hash, Eq, PartialEq, Debug, Display)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = i32;

        async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
            unimplemented!("test")
        }
    }

    #[tokio::test]
    async fn opaque_records_deps_when_used() {
        let dice = DiceModern::new(DiceData::new());

        let ctx = dice.updater().commit().await;

        let opaque = OpaqueValueModern::<K>::new(&ctx, DiceKey { index: 0 }, 1);

        assert_eq!(ctx.dep_trackers().recorded_deps(), &HashSet::default());

        assert_eq!(opaque.into_value(), 1);

        assert_eq!(
            ctx.dep_trackers().recorded_deps(),
            &[DiceKey { index: 0 }].into_iter().collect()
        );
    }
}
