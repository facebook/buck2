/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! End-to-end tests for DICE paging (`page_out` / on-demand page-in) through the
//! public API.

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;

use allocative::Allocative;
use async_trait::async_trait;
use derive_more::Display;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceComputations;
use dice::DiceEvent;
use dice::DiceEventListener;
use dice::DiceKeyDyn;
use dice::DiceStorage;
use dice::EqualityBehavior;
use dice::Key;
use dice::PagableStorageBackend;
use dice::UserComputationData;
use dice::ValueSerialize;
use dice_futures::cancellation::CancellationContext;
use pagable::Pagable;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::pagable_typetag;
use tempfile::tempdir;

/// A `ValueSerialize` that serializes successfully — so the node pages out — but
/// always fails to deserialize. This mimics a serialize/deserialize asymmetry
/// (e.g. a typetag mismatch) or on-disk corruption, exercising the worker's
/// page-in failure path.
struct FailToHydrateSerialize;

impl ValueSerialize for FailToHydrateSerialize {
    type Value = u64;

    fn pagable_serialize_value(
        &self,
        v: &Self::Value,
        ser: &mut dyn PagableSerializer,
    ) -> Option<pagable::Result<()>> {
        Some(v.pagable_serialize(ser))
    }

    fn pagable_deserialize_value<'de, D: PagableDeserializer<'de> + ?Sized>(
        &self,
        _deser: &mut D,
    ) -> pagable::Result<Self::Value> {
        Err(anyhow::anyhow!("simulated hydrate failure"))
    }
}

#[derive(Allocative, Clone, Copy, Debug, Display, PartialEq, Eq, Hash, Pagable)]
#[display("FailToHydrateKey({})", _0)]
#[pagable_typetag(DiceKeyDyn)]
struct FailToHydrateKey(u32);

#[async_trait]
impl Key for FailToHydrateKey {
    type Value = u64;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        u64::from(self.0) * 100
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        FailToHydrateSerialize
    }
}

/// Regression: when a paged-out value cannot be read back in, the awaiting
/// computation must not hang (previously the worker cancelled without ever
/// producing a result, leaving every awaiter blocked forever) and must recover by
/// recomputing.
#[tokio::test]
async fn failed_hydrate_of_paged_out_value_recomputes() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = {
        let mut builder = Dice::builder();
        builder.set_pagable_storage(storage);
        builder.build(DetectCycles::Disabled)
    };

    // Compute once so the value is resident, then page it out to disk.
    let tx = dice.updater().commit().await;
    let v1: u64 = *tx.compute(&FailToHydrateKey(7)).await?;
    assert_eq!(v1, 700);
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    // Looking the key up again pages it back in, but deserialization always fails.
    // The computation must recover by recomputing rather than hanging or erroring.
    // The paged-in value can never deserialize, so getting 700 back proves it was
    // recomputed.
    let tx = dice.updater().commit().await;
    let v2: u64 = *tokio::time::timeout(Duration::from_secs(10), tx.compute(&FailToHydrateKey(7)))
        .await
        .expect("compute must not hang when a paged-out value fails to hydrate")?;
    assert_eq!(v2, 700, "a failed hydrate should recompute the value");

    Ok(())
}

/// Captures `DiceEvent::HydrationFailed` key types so a test can assert a hydration
/// failure is reported out-of-band (rather than silently swallowed).
#[derive(Allocative)]
struct CapturingListener {
    #[allocative(skip)]
    hydration_failures: Arc<Mutex<Vec<String>>>,
}

impl DiceEventListener for CapturingListener {
    fn event(&self, ev: DiceEvent) {
        if let DiceEvent::HydrationFailed { key_type, .. } = ev {
            self.hydration_failures
                .lock()
                .unwrap()
                .push(key_type.to_owned());
        }
    }
}

/// A failed page-in reports a `HydrationFailed` event (which buck2 maps to a
/// `soft_error`), so the failure is visible in telemetry rather than lost.
#[tokio::test]
async fn failed_hydrate_reports_a_hydration_failed_event() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = {
        let mut builder = Dice::builder();
        builder.set_pagable_storage(storage);
        builder.build(DetectCycles::Disabled)
    };

    let tx = dice.updater().commit().await;
    let _: u64 = *tx.compute(&FailToHydrateKey(7)).await?;
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let captured = Arc::new(Mutex::new(Vec::new()));
    let mut data = UserComputationData::new();
    data.tracker = Arc::new(CapturingListener {
        hydration_failures: captured.clone(),
    });
    let tx = dice.updater_with_data(data).commit().await;
    let _ignored = tokio::time::timeout(Duration::from_secs(10), tx.compute(&FailToHydrateKey(7)))
        .await
        .expect("compute must not hang when a paged-out value fails to hydrate");

    let failures = captured.lock().unwrap();
    assert_eq!(
        failures.len(),
        1,
        "exactly one hydration failure should be reported, got {failures:?}"
    );
    assert!(
        failures[0].contains("FailToHydrateKey"),
        "reported key type should identify the failing key, got {:?}",
        failures[0]
    );

    Ok(())
}

/// A page-in failure must be repaired, not just reported: the recompute that recovers from
/// it has to replace the node, or the unreadable `DataKey` stays in the graph and every
/// later lookup pays another failed page-in (and files another `dice_hydration_failed`).
#[tokio::test]
async fn failed_hydrate_is_not_reported_again_on_a_later_lookup() -> anyhow::Result<()> {
    let tmp = tempdir()?;
    let storage = DiceStorage::open(tmp.path(), PagableStorageBackend::Sqlite)?;
    let dice = {
        let mut builder = Dice::builder();
        builder.set_pagable_storage(storage);
        builder.build(DetectCycles::Disabled)
    };

    let tx = dice.updater().commit().await;
    let _: u64 = *tx.compute(&FailToHydrateKey(7)).await?;
    drop(tx);

    dice.wait_for_idle().await;
    dice.page_out().await?;

    let failures_of = |dice: &Arc<Dice>| {
        let captured = Arc::new(Mutex::new(Vec::new()));
        let mut data = UserComputationData::new();
        data.tracker = Arc::new(CapturingListener {
            hydration_failures: captured.clone(),
        });
        (dice.updater_with_data(data), captured)
    };

    // First lookup after the page-out: hydration fails once and the value is recomputed.
    let (updater, first) = failures_of(&dice);
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&FailToHydrateKey(7)).await?, 700);
    drop(tx);
    dice.wait_for_idle().await;
    assert_eq!(first.lock().unwrap().len(), 1);

    // The recomputed value is resident, so a fresh transaction reads it without touching
    // storage at all.
    let (updater, second) = failures_of(&dice);
    let tx = updater.commit().await;
    assert_eq!(*tx.compute(&FailToHydrateKey(7)).await?, 700);
    assert_eq!(
        second.lock().unwrap().as_slice(),
        &[] as &[String],
        "the unreadable value should have been dropped by the first failure"
    );

    Ok(())
}
