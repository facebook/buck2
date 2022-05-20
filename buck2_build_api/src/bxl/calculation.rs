//! DICE calculations for bxl

use std::sync::Arc;

use async_trait::async_trait;
use buck2_core::result::{SharedResult, ToSharedResultExt};
use dice::{DiceComputations, Key};
use gazebo::prelude::*;

use crate::bxl::{eval::eval, result::BxlResult, BxlKey};

#[async_trait]
pub trait BxlCalculation {
    async fn eval_bxl<'a>(&self, bxl: BxlKey) -> SharedResult<Arc<BxlResult>>;
}

#[async_trait]
impl BxlCalculation for DiceComputations {
    async fn eval_bxl<'a>(&self, bxl: BxlKey) -> SharedResult<Arc<BxlResult>> {
        self.compute(&internal::BxlComputeKey(bxl)).await
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
    use derive_more::Display;
    use gazebo::prelude::*;

    use crate::bxl::BxlKey;

    #[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
    pub struct BxlComputeKey(pub BxlKey);
}

#[cfg(test)]
pub mod testing {
    pub use crate::bxl::calculation::internal::BxlComputeKey;
}
