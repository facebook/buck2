/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_futures::cancellation::CancellationContext;
use derive_more::Display;
use dupe::Dupe;

use crate::DiceDataBuilder;
use crate::DiceEvent;
use crate::DiceEventListener;
use crate::InjectedKey;
use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::dice::Dice;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;

#[derive(Default, Allocative)]
struct Tracker {
    state: Mutex<Vec<DiceEvent>>,
}

impl DiceEventListener for Tracker {
    fn event(&self, event: DiceEvent) {
        self.state.lock().unwrap().push(event);
    }
}

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq, Allocative)]
#[display("{:?}", self)]
struct Injected;

#[async_trait]
impl InjectedKey for Injected {
    type Value = i32;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
#[display("{:?}", self)]
struct Stage0;

#[async_trait]
impl Key for Stage0 {
    type Value = ();

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.compute(&Injected).await.unwrap();
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

#[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
#[display("{:?}", self)]
struct Stage1;

#[async_trait]
impl Key for Stage1 {
    type Value = ();

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.compute(&Stage0).await.unwrap()
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

async fn test_events_impl(builder: DiceDataBuilder) -> anyhow::Result<()> {
    let dice = builder.build(DetectCycles::Enabled);

    {
        let tracker = Arc::new(Tracker::default());

        let data = UserComputationData {
            tracker: tracker.dupe(),
            ..Default::default()
        };

        let mut updater = dice.updater_with_data(data);
        updater.changed_to(vec![(Injected, 123)])?;

        let mut transaction = updater.commit().await;
        transaction.compute(&Stage1).await?;

        assert_eq!(
            &*tracker.state.lock().unwrap(),
            &[
                DiceEvent::Started { key_type: "Stage1" },
                DiceEvent::ComputeStarted { key_type: "Stage1" },
                DiceEvent::Started { key_type: "Stage0" },
                DiceEvent::ComputeStarted { key_type: "Stage0" },
                DiceEvent::ComputeFinished { key_type: "Stage0" },
                DiceEvent::Finished { key_type: "Stage0" },
                DiceEvent::ComputeFinished { key_type: "Stage1" },
                DiceEvent::Finished { key_type: "Stage1" },
            ]
        );
    }

    {
        let tracker = Arc::new(Tracker::default());

        let data = UserComputationData {
            tracker: tracker.dupe(),
            ..Default::default()
        };

        // Change the value.
        let mut updater = dice.updater_with_data(data);
        updater.changed_to(vec![(Injected, 456)])?;

        let mut transaction = updater.commit().await;
        transaction.compute(&Stage1).await?;

        assert_eq!(
            &*tracker.state.lock().unwrap(),
            &[
                DiceEvent::Started { key_type: "Stage1" },
                DiceEvent::CheckDepsStarted { key_type: "Stage1" },
                DiceEvent::Started { key_type: "Stage0" },
                DiceEvent::CheckDepsStarted { key_type: "Stage0" },
                DiceEvent::CheckDepsFinished { key_type: "Stage0" },
                DiceEvent::ComputeStarted { key_type: "Stage0" },
                DiceEvent::ComputeFinished { key_type: "Stage0" },
                DiceEvent::Finished { key_type: "Stage0" },
                DiceEvent::CheckDepsFinished { key_type: "Stage1" },
                DiceEvent::Finished { key_type: "Stage1" },
            ]
        );
    }

    Ok(())
}

#[tokio::test]
async fn test_events_legacy() -> anyhow::Result<()> {
    test_events_impl(Dice::builder()).await
}

#[tokio::test]
async fn test_events_modern() -> anyhow::Result<()> {
    test_events_impl(Dice::modern()).await
}
