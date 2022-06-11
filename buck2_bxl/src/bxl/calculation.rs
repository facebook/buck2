/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::bxl::{calculation::BxlCalculationDyn, result::BxlResult, BxlKey};
use buck2_core::result::{SharedResult, ToSharedResultExt};
use dice::{DiceComputations, Key};
use gazebo::dupe::Dupe;

use crate::bxl::eval::eval;

pub struct BxlCalculationImpl;

#[async_trait]
impl BxlCalculationDyn for BxlCalculationImpl {
    async fn eval_ctx(&self, ctx: &DiceComputations, bxl: BxlKey) -> SharedResult<Arc<BxlResult>> {
        ctx.compute(&internal::BxlComputeKey(bxl)).await
    }
}

#[async_trait]
impl Key for internal::BxlComputeKey {
    type Value = SharedResult<Arc<BxlResult>>;

    async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
        let key = self.0.dupe();
        ctx.temporary_spawn(async move |ctx| eval(ctx, key).await.shared_error().map(Arc::new))
            .await
    }

    fn equality(_: &Self::Value, _: &Self::Value) -> bool {
        false
    }

    fn validity(res: &Self::Value) -> bool {
        // cheap hack to not cache bxl when printing
        // TODO(T120858909) properly cache prints
        res.as_ref().map_or(true, |res| !res.has_print())
    }
}

mod internal {
    use buck2_build_api::bxl::BxlKey;
    use derive_more::Display;
    use gazebo::prelude::*;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    pub struct BxlComputeKey(pub BxlKey);
}

#[cfg(test)]
pub mod testing {
    pub use crate::bxl::calculation::internal::BxlComputeKey;
}
