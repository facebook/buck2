/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::impls::dice::DiceModern;

#[tokio::test]
async fn different_data_per_compute_ctx() {
    struct U(usize);

    #[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
    #[display(fmt = "{:?}", self)]
    struct DataRequest(u8);
    #[async_trait]
    impl Key for DataRequest {
        type Value = usize;

        async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
            ctx.per_transaction_data().data.get::<U>().unwrap().0
        }

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }
    }

    let dice = DiceModern::builder().build(DetectCycles::Enabled);
    let per_cmd_data0 = {
        let mut d = UserComputationData::new();
        d.data.set(U(0));
        d
    };
    let per_cmd_data1 = {
        let mut d = UserComputationData::new();
        d.data.set(U(1));
        d
    };

    let ctx0 = dice.updater_with_data(per_cmd_data0).commit().await;

    let ctx1 = dice.updater_with_data(per_cmd_data1).commit().await;

    let request0 = ctx0.compute(&DataRequest(0));
    let request1 = ctx1.compute(&DataRequest(1));

    assert_eq!(request0.await.unwrap(), 0);
    assert_eq!(request1.await.unwrap(), 1);
}
