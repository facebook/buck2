/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::sync::Arc;
use std::sync::Mutex;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dupe::Dupe;
use more_futures::cancellation::CancellationContext;

use crate::api::computations::DiceComputations;
use crate::api::cycles::DetectCycles;
use crate::api::dice::Dice;
use crate::api::key::Key;
use crate::api::user_data::UserComputationData;
use crate::ActivationData;
use crate::ActivationTracker;
use crate::DiceDataBuilder;
use crate::InjectedKey;

#[derive(Default, Allocative)]
struct Tracker {
    /// Key, deps, data, reused
    state: Mutex<Vec<(Kind, Vec<Kind>, Option<Data>, bool)>>,
}

impl Tracker {
    fn new() -> Self {
        Self {
            state: Mutex::new(Vec::new()),
        }
    }
}

impl ActivationTracker for Tracker {
    fn key_activated(
        &self,
        key: &dyn Any,
        deps: &mut dyn Iterator<Item = &dyn Any>,
        activation_data: ActivationData,
    ) {
        let (data, reused) = match activation_data {
            ActivationData::Evaluated(d) => (d.map(|d| *d.downcast::<Data>().unwrap()), false),
            ActivationData::Reused => (None, true),
        };

        self.state.lock().unwrap().push((
            Kind::from_any(key),
            deps.into_iter().map(Kind::from_any).collect(),
            data,
            reused,
        ));
    }
}

#[derive(PartialEq, Eq, Debug, Dupe, Clone, Allocative)]
enum Kind {
    Injected,
    Stage0,
    Stage1,
}

impl Kind {
    fn from_any(key: &dyn Any) -> Self {
        if key.is::<Injected>() {
            return Self::Injected;
        }

        if key.is::<Stage0>() {
            return Self::Stage0;
        }

        if key.is::<Stage1>() {
            return Self::Stage1;
        }

        panic!("Unexpected key: {:?}", key)
    }
}

#[derive(PartialEq, Eq, Debug, Dupe, Clone, Allocative)]
struct Data;

#[derive(Clone, Dupe, Debug, Display, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct Injected;

#[async_trait]
impl InjectedKey for Injected {
    type Value = i32;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
#[display(fmt = "{:?}", self)]
struct Stage0;

#[async_trait]
impl Key for Stage0 {
    type Value = ();

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.store_evaluation_data(Data).unwrap();
        ctx.compute(&Injected).await.unwrap();
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

#[derive(Clone, Dupe, Debug, Display, PartialEq, Eq, Hash, Allocative)]
#[display(fmt = "{:?}", self)]
struct Stage1;

#[async_trait]
impl Key for Stage1 {
    type Value = ();

    async fn compute(
        &self,
        ctx: &DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        ctx.store_evaluation_data(Data).unwrap();
        ctx.compute(&Stage0).await.unwrap()
    }

    fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
        true
    }
}

async fn test_events_impl(builder: DiceDataBuilder) -> anyhow::Result<()> {
    let dice = builder.build(DetectCycles::Enabled);

    {
        let activation_tracker = Arc::new(Tracker::new());

        let data = UserComputationData {
            activation_tracker: Some(activation_tracker.dupe()),
            ..Default::default()
        };

        let mut updater = dice.updater_with_data(data);
        updater.changed_to(vec![(Injected, 123)])?;

        let transaction = updater.commit().await;
        transaction.compute(&Stage1).await?;

        assert_eq!(
            &*activation_tracker.state.lock().unwrap(),
            &[
                (Kind::Stage0, vec![Kind::Injected], Some(Data), false),
                (Kind::Stage1, vec![Kind::Stage0], Some(Data), false),
            ]
        );
    }

    {
        let activation_tracker = Arc::new(Tracker::default());

        let data = UserComputationData {
            activation_tracker: Some(activation_tracker.dupe()),
            ..Default::default()
        };

        // Change the value.
        let mut updater = dice.updater_with_data(data);
        updater.changed_to(vec![(Injected, 456)])?;

        let transaction = updater.commit().await;
        transaction.compute(&Stage1).await?;

        assert_eq!(
            &*activation_tracker.state.lock().unwrap(),
            &[
                (Kind::Stage0, vec![Kind::Injected], Some(Data), false),
                (Kind::Stage1, vec![Kind::Stage0], None, true),
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
