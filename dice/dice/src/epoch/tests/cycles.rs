/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Cycle detection: what a detected cycle does to the tasks in it.

use std::sync::Arc;
use std::task::Poll;

use allocative::Allocative;
use assert_matches::assert_matches;
use async_trait::async_trait;
use derive_more::Display;
use dice_futures::cancellation::CancellationContext;
use dice_futures::spawner::TokioSpawner;
use dupe::Dupe;
use futures::FutureExt;
use futures::poll;
use pagable::Pagable;
use pagable::pagable_typetag;
use tokio::sync::Notify;

use crate::DiceKeyDyn;
use crate::EqualityBehavior;
use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::ValueSerialize;
use crate::core::graph::revision::Revision;
use crate::epoch::cycles::DetectedCycle;
use crate::epoch::task::dice::DiceTask;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::task::spawn_dice_task;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;
use crate::value::DiceKeyValue;
use crate::value::DiceValidValue;
use crate::value::MaybeResidentComputedValue;
use crate::value::MaybeValidDiceValue;
use crate::value::TrackedInvalidationPaths;

// A detected cycle, and what it does to the tasks in it.

/// A key that displays as `key<n>`, for cycles built by hand.
#[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash, Pagable)]
#[display("key{}", _0)]
#[pagable_typetag(DiceKeyDyn)]
struct N(u32);

#[async_trait]
impl Key for N {
    type Value = u32;

    async fn compute(
        &self,
        _ctx: &mut DiceComputations,
        _cancellations: &CancellationContext,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|x, y| x == y)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

fn key(i: u32) -> DiceKey {
    DiceKey { index: i }
}

/// A cycle of the given keys, each with whether its waits fail, with the given waits among them.
fn cycle_of(members: &[(u32, bool)], edges: &[(u32, u32)]) -> Arc<DetectedCycle> {
    DetectedCycle::new(
        members
            .iter()
            .map(|&(i, fails)| (key(i), DiceKeyErased::key(N(i)), fails)),
        edges.iter().map(|&(a, b)| (key(a), key(b))),
    )
}

fn error_message(cycle: &DetectedCycle, parent: u32, awaited: u32) -> Option<String> {
    cycle
        .error_for(ParentKey::Some(key(parent)), key(awaited))
        .map(|e| e.to_string())
}

#[test]
fn errors_describe_a_shortest_cycle_through_the_failing_wait() {
    // 1 -> 2 -> 3 -> 1, plus a shortcut 2 -> 1.
    let cycle = cycle_of(
        &[(1, true), (2, true), (3, true)],
        &[(1, 2), (2, 3), (3, 1), (2, 1)],
    );
    assert_eq!(
        error_message(&cycle, 1, 2).unwrap(),
        "Dependency cycle detected (`->` means \"depends on\"):\n  key1 ->\n  key2 ->\n  key1"
    );
    assert_eq!(
        error_message(&cycle, 3, 1).unwrap(),
        "Dependency cycle detected (`->` means \"depends on\"):\n  key3 ->\n  key1 ->\n  key2 ->\n  key3"
    );
}

#[test]
fn a_key_waiting_on_itself_is_a_cycle() {
    let cycle = cycle_of(&[(7, true)], &[(7, 7)]);
    assert_eq!(
        error_message(&cycle, 7, 7).unwrap(),
        "Dependency cycle detected (`->` means \"depends on\"):\n  key7 ->\n  key7"
    );
}

#[test]
fn only_failing_members_get_errors() {
    let cycle = cycle_of(&[(1, true), (2, false)], &[(1, 2), (2, 1)]);
    assert!(error_message(&cycle, 1, 2).is_some());
    assert!(
        error_message(&cycle, 2, 1).is_none(),
        "member whose waits don't fail"
    );
    assert!(error_message(&cycle, 3, 1).is_none(), "not a member");
    assert!(
        cycle.error_for(ParentKey::None, key(1)).is_none(),
        "not a computation"
    );
}

/// A task for key 10 that finishes with the value 2 once `release` is notified.
fn spawn_releasable_task(release: &Arc<Notify>) -> (DiceTask, DicePromise<'static>) {
    let release = release.dupe();
    spawn_dice_task(key(10), &TokioSpawner, &(), move |handle| {
        async move {
            release.notified().await;
            handle.finished(Ok(MaybeResidentComputedValue::new_resident(
                MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<N>::new(2))),
                TrackedInvalidationPaths::clean(),
                Revision::FIRST,
            )));
        }
        .boxed()
    })
}

fn is_value(v: &MaybeResidentComputedValue, expected: u32) -> bool {
    v.testing_resident_value()
        .equality(&DiceValidValue::testing_new(DiceKeyValue::<N>::new(
            expected,
        )))
}

#[tokio::test]
async fn a_cycle_fails_the_waits_of_its_keys_and_serves_the_others() -> anyhow::Result<()> {
    let release = Arc::new(Notify::new());
    let (task, _initial_promise) = spawn_releasable_task(&release);
    let cycle = cycle_of(&[(1, true), (10, true)], &[(1, 10), (10, 1)]);

    let mut member = Box::pin(task.depended_on_by(ParentKey::Some(key(1))).unwrap());
    let mut outsider = Box::pin(task.depended_on_by(ParentKey::Some(key(2))).unwrap());
    assert!(poll!(&mut member).is_pending());
    assert!(poll!(&mut outsider).is_pending());

    task.as_ref().mark_in_cycle(&cycle);

    assert_matches!(poll!(&mut member), Poll::Ready(Err(e)) => {
        assert_eq!(
            e.to_string(),
            "Dependency cycle detected (`->` means \"depends on\"):\n  key1 ->\n  key10 ->\n  key1"
        );
    });
    // Asking again comes to the same.
    assert_matches!(
        task.depended_on_by(ParentKey::Some(key(1))).unwrap().await,
        Err(_)
    );
    assert!(poll!(&mut outsider).is_pending());

    // The task finishes as usual, and that is what the outsider gets.
    release.notify_one();
    assert!(is_value(outsider.await?, 2));

    Ok(())
}

#[tokio::test]
async fn a_waiting_key_of_a_cycle_gets_the_error_even_if_the_value_arrives_first()
-> anyhow::Result<()> {
    let release = Arc::new(Notify::new());
    let (task, _initial_promise) = spawn_releasable_task(&release);

    let mut member = Box::pin(task.depended_on_by(ParentKey::Some(key(1))).unwrap());
    assert!(poll!(&mut member).is_pending());

    task.as_ref()
        .mark_in_cycle(&cycle_of(&[(1, true), (10, true)], &[(1, 10), (10, 1)]));
    // Let the task finish before the member gets to look again.
    release.notify_one();
    task.depended_on_by(ParentKey::None).unwrap().await?;

    assert_matches!(poll!(&mut member), Poll::Ready(Err(_)));
    // A key of the cycle that only comes asking now gets the value like anyone else.
    assert!(is_value(
        task.depended_on_by(ParentKey::Some(key(1)))
            .unwrap()
            .await?,
        2
    ));

    Ok(())
}
