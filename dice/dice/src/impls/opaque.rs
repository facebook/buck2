/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use derivative::Derivative;
use dupe::Dupe;

use crate::api::error::DiceResult;
use crate::api::key::Key;
use crate::api::projection::ProjectionKey;
use crate::impls::ctx::PerComputeCtx;
use crate::impls::key::DiceKey;
use crate::impls::value::MaybeValidDiceValue;

#[derive(Derivative)]
#[derivative(Debug)]
pub(crate) struct OpaqueValueModern<'a, K: Key> {
    derive_from_key: DiceKey,
    #[derivative(Debug = "ignore")]
    derive_from: MaybeValidDiceValue,
    #[derivative(Debug = "ignore")]
    parent_computation: &'a PerComputeCtx,
    ty: PhantomData<K>,
}

impl<'a, K> OpaqueValueModern<'a, K>
where
    K: Key,
{
    pub(crate) fn new(
        parent_computation: &'a PerComputeCtx,
        derive_from_key: DiceKey,
        derive_from: MaybeValidDiceValue,
    ) -> Self {
        Self {
            derive_from_key,
            derive_from,
            parent_computation,
            ty: Default::default(),
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
        self.parent_computation
            .dep_trackers()
            .record(self.derive_from_key, self.derive_from.validity());

        self.derive_from
            .downcast_maybe_transient::<K::Value>()
            .expect("type mismatch")
            .dupe()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use more_futures::cancellation::CancellationContext;

    use crate::api::data::DiceData;
    use crate::api::key::Key;
    use crate::impls::dep_trackers::testing::RecordingDepsTrackersExt;
    use crate::impls::dice::DiceModern;
    use crate::impls::key::DiceKey;
    use crate::impls::opaque::OpaqueValueModern;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidity;
    use crate::impls::value::MaybeValidDiceValue;
    use crate::DiceComputations;
    use crate::HashSet;

    #[derive(Allocative, Clone, Hash, Eq, PartialEq, Debug, Display)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = i32;

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

    #[tokio::test]
    async fn opaque_records_deps_when_used() {
        let dice = DiceModern::new(DiceData::new());

        let ctx = dice.updater().commit().await;

        let opaque = OpaqueValueModern::<K>::new(
            &ctx,
            DiceKey { index: 0 },
            MaybeValidDiceValue::new(Arc::new(DiceKeyValue::<K>::new(1)), DiceValidity::Valid),
        );

        assert_eq!(ctx.dep_trackers().recorded_deps(), &HashSet::default());

        assert_eq!(opaque.into_value(), 1);

        assert_eq!(
            ctx.dep_trackers().recorded_deps(),
            &[DiceKey { index: 0 }].into_iter().collect()
        );
    }
}
