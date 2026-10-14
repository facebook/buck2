/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Cycle detection: what a detected cycle does to the tasks in it, and how cycles are found among
//! a version's tasks.

use std::pin::Pin;
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
use crate::DiceProjectionDyn;
use crate::EqualityBehavior;
use crate::api::computations::DiceComputations;
use crate::api::key::Key;
use crate::api::key::NoValueSerialize;
use crate::api::key::ValueSerialize;
use crate::api::projection::DiceProjectionComputations;
use crate::api::projection::ProjectionKey;
use crate::core::graph::revision::Revision;
use crate::epoch::cache::SharedCache;
use crate::epoch::cycles::DetectedCycle;
use crate::epoch::cycles::find_cycles;
use crate::epoch::cycles::mark_cycles;
use crate::epoch::cycles::strongly_connected_components;
use crate::epoch::task::dice::DiceTask;
use crate::epoch::task::promise::DicePromise;
use crate::epoch::task::spawn_dice_task;
use crate::key::CowDiceKeyHashed;
use crate::key::DiceKey;
use crate::key::DiceKeyErased;
use crate::key::ParentKey;
use crate::key_index::DiceKeyIndex;
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

// Finding the cycles among a version's tasks.

#[test]
fn components_are_strongly_connected() {
    // 0 <-> 1 -> 2 <-> 3, 4 on its own, 5 -> 5.
    let edges = [(0, 1), (1, 0), (1, 2), (2, 3), (3, 2), (5, 5)];
    let (components, component_of) = strongly_connected_components(6, &edges);
    for (c, nodes) in components.iter().enumerate() {
        for &n in nodes {
            assert_eq!(component_of[n as usize], c as u32);
        }
    }
    let mut components: Vec<Vec<u32>> = components
        .into_iter()
        .map(|mut c| {
            c.sort();
            c
        })
        .collect();
    components.sort();
    assert_eq!(components, vec![vec![0, 1], vec![2, 3], vec![4], vec![5]]);
}

#[test]
fn long_chains_do_not_overflow_the_stack() {
    let n = 1_000_000u32;
    let mut edges: Vec<(u32, u32)> = (0..n - 1).map(|i| (i, i + 1)).collect();
    edges.push((n - 1, 0));
    let (components, _) = strongly_connected_components(n as usize, &edges);
    assert_eq!(components.len(), 1);
}

/// A task for `key` that runs until the returned dependent is dropped.
fn running_task(cache: &SharedCache, key: DiceKey) -> (DiceTask, DicePromise<'static>) {
    let (task, promise) = spawn_dice_task(key, &TokioSpawner, &(), |handle| {
        async move {
            let _handle = handle;
            std::future::pending().await
        }
        .boxed()
    });
    cache.testing_insert_task(key, task.dupe());
    (task, promise)
}

/// Makes `waiting` wait on `task`: the wait counts once its promise has been polled.
async fn wait(task: &DiceTask, waiting: DiceKey) -> Pin<Box<DicePromise<'_>>> {
    let mut promise = Box::pin(task.depended_on_by(ParentKey::Some(waiting)).unwrap());
    assert!(poll!(promise.as_mut()).is_pending());
    promise
}

/// The keys of `cycle`, sorted.
fn members(cycle: &DetectedCycle) -> Vec<u32> {
    cycle.keys().map(|k| k.index).collect()
}

fn sorted(mut keys: Vec<u32>) -> Vec<u32> {
    keys.sort();
    keys
}

#[tokio::test]
async fn finds_the_cycles_nothing_can_resolve() {
    let key_index = DiceKeyIndex::default();
    let cache = SharedCache::new();
    let keys: Vec<DiceKey> = (0..5).map(|i| key_index.index_key(N(i))).collect();
    let tasks: Vec<_> = keys.iter().map(|&k| running_task(&cache, k)).collect();
    let task = |i: usize| &tasks[i].0;

    // 1 and 2 wait on each other, but 2 also waits on 3, which is running on its own:
    // whatever 3 ends up as may let 2 go, so this is not a cycle yet. 0 waiting on 1 from
    // the outside changes nothing either way.
    let _w12 = wait(task(2), keys[1]).await;
    let _w21 = wait(task(1), keys[2]).await;
    let w23 = wait(task(3), keys[2]).await;
    let _w01 = wait(task(1), keys[0]).await;
    assert!(find_cycles(&cache, &key_index).is_empty());

    drop(w23);
    let cycles = find_cycles(&cache, &key_index);
    assert_eq!(cycles.len(), 1);
    assert_eq!(
        members(&cycles[0]),
        sorted(vec![keys[1].index, keys[2].index])
    );

    // 4 waits on itself.
    let _w44 = wait(task(4), keys[4]).await;
    let cycles = find_cycles(&cache, &key_index);
    let mut found: Vec<Vec<u32>> = cycles.iter().map(|c| members(c)).collect();
    found.sort();
    let mut expected = vec![
        sorted(vec![keys[1].index, keys[2].index]),
        vec![keys[4].index],
    ];
    expected.sort();
    assert_eq!(found, expected);
}

#[tokio::test]
async fn marking_fails_the_waits_of_the_cycle() {
    let key_index = DiceKeyIndex::default();
    let cache = SharedCache::new();
    let keys: Vec<DiceKey> = (0..3).map(|i| key_index.index_key(N(i))).collect();
    let tasks: Vec<_> = keys.iter().map(|&k| running_task(&cache, k)).collect();
    let task = |i: usize| &tasks[i].0;

    let mut w12 = wait(task(2), keys[1]).await;
    let mut w21 = wait(task(1), keys[2]).await;
    let mut w01 = wait(task(1), keys[0]).await;

    let cycles = find_cycles(&cache, &key_index);
    mark_cycles(&cache, &cycles);

    assert_matches!(poll!(w12.as_mut()), Poll::Ready(Err(_)));
    assert_matches!(poll!(w21.as_mut()), Poll::Ready(Err(_)));
    assert!(poll!(w01.as_mut()).is_pending(), "not a key of the cycle");
}

/// A projection of `N`, for cycles through a projection key's task.
#[derive(Allocative, Debug, Display, Clone, Dupe, Eq, PartialEq, Hash, Pagable)]
#[pagable_typetag(DiceProjectionDyn)]
struct P;

impl ProjectionKey for P {
    type DeriveFromKey = N;
    type Value = ();

    fn compute(
        &self,
        _derive_from: &<<Self as ProjectionKey>::DeriveFromKey as Key>::Value,
        _ctx: &DiceProjectionComputations,
    ) -> Self::Value {
        unimplemented!("test")
    }

    fn equality_behavior() -> EqualityBehavior<Self::Value> {
        EqualityBehavior::Compare(|_, _| true)
    }

    fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
        NoValueSerialize::<Self::Value>::new()
    }
}

/// A projection key's task only exists for dep checks, and only ever waits for its base (see
/// the projection arm of `TransactionData::evaluate`). It is a node of the wait graph like any
/// other, but the detector does not fail its wait: that wait is served the base's value instead,
/// once failing the regular keys' waits has broken the cycle.
#[tokio::test]
async fn a_projection_key_in_a_cycle_keeps_waiting_for_its_base() {
    let key_index = DiceKeyIndex::default();
    let cache = SharedCache::new();
    let q = key_index.index_key(N(0));
    let base = key_index.index_key(N(1));
    let proj = key_index.index(CowDiceKeyHashed::proj_ref(base, &P));
    let (q_task, _q_promise) = running_task(&cache, q);
    let (base_task, _base_promise) = running_task(&cache, base);
    let (proj_task, _proj_promise) = running_task(&cache, proj);

    // q is checking a dep on the projection, whose task waits for the base, whose compute
    // requested q.
    let mut q_on_proj = wait(&proj_task, q).await;
    let mut proj_on_base = wait(&base_task, proj).await;
    let mut base_on_q = wait(&q_task, base).await;

    let cycles = find_cycles(&cache, &key_index);
    assert_eq!(cycles.len(), 1);
    assert_eq!(
        members(&cycles[0]),
        sorted(vec![q.index, base.index, proj.index])
    );
    mark_cycles(&cache, &cycles);

    assert_matches!(poll!(q_on_proj.as_mut()), Poll::Ready(Err(_)));
    assert_matches!(poll!(base_on_q.as_mut()), Poll::Ready(Err(_)));
    assert!(poll!(proj_on_base.as_mut()).is_pending());
}
