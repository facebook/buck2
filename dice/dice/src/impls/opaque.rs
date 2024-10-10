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

use crate::api::key::Key;
use crate::impls::key::DiceKey;
use crate::impls::value::MaybeValidDiceValue;
use crate::impls::value::TrackedInvalidationPaths;

#[derive(Derivative)]
#[derivative(Debug)]
pub(crate) struct OpaqueValueModern<K: Key> {
    pub(crate) derive_from_key: DiceKey,
    #[derivative(Debug = "ignore")]
    pub(crate) derive_from: MaybeValidDiceValue,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
    ty: PhantomData<K>,
}

impl<K> OpaqueValueModern<K>
where
    K: Key,
{
    pub(crate) fn new(
        derive_from_key: DiceKey,
        derive_from: MaybeValidDiceValue,
        invalidation_paths: TrackedInvalidationPaths,
    ) -> Self {
        Self {
            derive_from_key,
            derive_from,
            invalidation_paths,
            ty: Default::default(),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use allocative::Allocative;
    use async_trait::async_trait;
    use buck2_futures::cancellation::CancellationContext;
    use derive_more::Display;

    use crate::api::data::DiceData;
    use crate::api::key::Key;
    use crate::impls::deps::testing::RecordingDepsTrackersExt;
    use crate::impls::dice::DiceModern;
    use crate::impls::key::DiceKey;
    use crate::impls::opaque::OpaqueValueModern;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidity;
    use crate::impls::value::MaybeValidDiceValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::DiceComputations;
    use crate::HashSet;

    #[derive(Allocative, Clone, Hash, Eq, PartialEq, Debug, Display)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = i32;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
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

        let mut ctx = dice.updater().commit().await;

        let opaque = OpaqueValueModern::<K>::new(
            DiceKey { index: 0 },
            MaybeValidDiceValue::new(Arc::new(DiceKeyValue::<K>::new(1)), DiceValidity::Valid),
            TrackedInvalidationPaths::clean(),
        );

        assert_eq!(ctx.dep_trackers().recorded_deps(), HashSet::default());

        assert_eq!(ctx.opaque_into_value(opaque), 1);

        assert_eq!(
            ctx.dep_trackers().recorded_deps(),
            [DiceKey { index: 0 }].into_iter().collect()
        );
    }
}
