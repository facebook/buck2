/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Shared, concurrent dice task cache that is shared between computations at the same version

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dashmap::DashMap;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use fxhash::FxBuildHasher;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::value::DiceComputedValue;

#[derive(Allocative)]
struct Data {
    completed: ShardedLockFreeRawTable<Arc<DiceCompletedTask>, 64>,
    /// Completed tasks lazily moved into `completed` from this map.
    storage: DashMap<DiceKey, DiceTask, FxBuildHasher>,
    is_cancelled: AtomicBool,
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct SharedCache {
    data: Arc<Data>,
}

#[derive(Allocative)]
struct DiceCompletedTask {
    key: DiceKey,
    value: DiceComputedValue,
}

/// Reference to the task in the cache.
pub(crate) enum DiceTaskRef<'a> {
    Computed(DiceComputedValue),
    Occupied(dashmap::mapref::entry::OccupiedEntry<'a, DiceKey, DiceTask>),
    Vacant(dashmap::mapref::entry::VacantEntry<'a, DiceKey, DiceTask>),
    TransactionCancelled,
}

impl DiceTaskRef<'_> {
    #[cfg(test)]
    pub(crate) fn testing_insert(self, task: DiceTask) {
        if let Self::Vacant(e) = self {
            e.insert(task);
        } else {
            panic!("inserting into non-vacant entry");
        }
    }
}

impl SharedCache {
    fn key_hash(key: DiceKey) -> u64 {
        (key.index as u64).wrapping_mul(0x9e3779b97f4a7c15)
    }

    fn try_get_computed(&self, key: DiceKey) -> Option<DiceComputedValue> {
        let hash = Self::key_hash(key);
        self.data
            .completed
            .lookup(hash, |task| task.key == key)
            .map(|task| task.value.dupe())
    }

    pub(crate) fn get(&self, key: DiceKey) -> DiceTaskRef<'_> {
        if let Some(computed) = self.try_get_computed(key) {
            return DiceTaskRef::Computed(computed);
        }

        let entry = self.data.storage.entry(key);

        // Not we acquired the lock, check computed map again.
        let computed = self.try_get_computed(key);

        if let Some(computed) = computed {
            return DiceTaskRef::Computed(computed);
        }

        let working_entry = match entry {
            dashmap::mapref::entry::Entry::Occupied(e) => {
                if let Some(Ok(result)) = e.get().get_finished_value() {
                    // Promote entry to computed.
                    // So lookup will be faster next time.

                    // TODO(nga): insert unique unchecked,
                    //   which `LockFreeRawTable` does not support yet.
                    let (_ignore, original) = self.data.completed.insert(
                        Self::key_hash(key),
                        Arc::new(DiceCompletedTask {
                            key,
                            value: result.dupe(),
                        }),
                        |a, b| a.key == b.key,
                        |task| Self::key_hash(task.key),
                    );
                    assert!(original.is_none());

                    // Must remove from dashmap after inserting into completed.
                    e.remove();
                    return DiceTaskRef::Computed(result);
                }
                DiceTaskRef::Occupied(e)
            }
            dashmap::mapref::entry::Entry::Vacant(e) => DiceTaskRef::Vacant(e),
        };

        if self.data.is_cancelled.load(Ordering::Acquire) {
            return DiceTaskRef::TransactionCancelled;
        }

        working_entry
    }

    pub(crate) fn new() -> Self {
        SharedCache {
            data: Arc::new(Data {
                storage: DashMap::default(),
                completed: ShardedLockFreeRawTable::new(),
                is_cancelled: AtomicBool::new(false),
            }),
        }
    }

    pub(crate) fn active_tasks_count(&self) -> usize {
        self.data.storage.len() + self.data.completed.len()
    }

    /// This function gets the termination observer for all running tasks when transaction is
    /// cancelled and prevents further tasks from being added
    pub(crate) fn cancel_pending_tasks(self) -> Vec<DiceTask> {
        self.data.is_cancelled.store(true, Ordering::Release);
        self.data
            .storage
            .iter()
            .filter_map(|entry| {
                if entry.value().is_pending() {
                    entry.value().cancel(CancellationReason::TransactionDropped);
                    Some(entry.value().clone())
                } else {
                    None
                }
            })
            .collect()
    }
}

#[cfg(test)]
impl SharedCache {
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }
}

pub(crate) mod introspection {
    use crate::impls::cache::SharedCache;
    use crate::impls::key::DiceKey;
    use crate::legacy::dice_futures::dice_task::DiceTaskStateForDebugging;

    impl SharedCache {
        pub(crate) fn iter_tasks(
            &self,
        ) -> impl Iterator<Item = (DiceKey, DiceTaskStateForDebugging)> {
            self.data
                .storage
                .iter()
                .map(|entry| (*entry.key(), entry.value().introspect_state()))
        }
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dice_error::result::CancellationReason;
    use dice_futures::cancellation::CancellationContext;
    use dice_futures::spawner::TokioSpawner;
    use dupe::Dupe;
    use futures::FutureExt;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::arc::Arc;
    use crate::impls::cache::DiceTaskRef;
    use crate::impls::cache::SharedCache;
    use crate::impls::key::DiceKey;
    use crate::impls::key::ParentKey;
    use crate::impls::task::dice::DiceTask;
    use crate::impls::task::spawn_dice_task;
    use crate::impls::value::DiceComputedValue;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::MaybeValidDiceValue;
    use crate::impls::value::TrackedInvalidationPaths;
    use crate::versions::VersionRanges;

    #[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &mut DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            true
        }
    }

    async fn make_completed_task(key: DiceKey, val: usize) -> DiceTask {
        let task = spawn_dice_task(key, &TokioSpawner, &(), |handle| {
            async move {
                handle.finished(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(val),
                    )),
                    Arc::new(VersionRanges::new()),
                    TrackedInvalidationPaths::clean(),
                ));

                Box::new(()) as Box<dyn Any + Send>
            }
            .boxed()
        });

        task.depended_on_by(ParentKey::None).unwrap().await.unwrap();

        task
    }

    async fn make_finished_cancelling_task(key: DiceKey) -> DiceTask {
        let finished_cancelling_tasks = spawn_dice_task(key, &TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                futures::future::pending().await
            }
            .boxed()
        });
        finished_cancelling_tasks.cancel(CancellationReason::ByTest);

        finished_cancelling_tasks.await_termination().await;

        finished_cancelling_tasks
    }

    fn make_never_finish_yet_to_cancel_task(key: DiceKey) -> DiceTask {
        spawn_dice_task(key, &TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                futures::future::pending().await
            }
            .boxed()
        })
    }

    #[tokio::test]
    async fn test_drain_task() {
        let cache = SharedCache::new();

        let completed_task1 = make_completed_task(DiceKey { index: 10 }, 1).await;
        let completed_task2 = make_completed_task(DiceKey { index: 20 }, 2).await;

        let finished_cancelling_tasks1 = make_finished_cancelling_task(DiceKey { index: 30 }).await;
        let finished_cancelling_tasks2 = make_finished_cancelling_task(DiceKey { index: 40 }).await;

        let yet_to_cancel_tasks1 = make_never_finish_yet_to_cancel_task(DiceKey { index: 50 });
        let yet_to_cancel_tasks2 = make_never_finish_yet_to_cancel_task(DiceKey { index: 60 });
        let yet_to_cancel_tasks3 = make_never_finish_yet_to_cancel_task(DiceKey { index: 70 });

        cache
            .get(DiceKey { index: 1 })
            .testing_insert(completed_task1);
        cache
            .get(DiceKey { index: 2 })
            .testing_insert(completed_task2);
        cache
            .get(DiceKey { index: 3 })
            .testing_insert(finished_cancelling_tasks1);
        cache
            .get(DiceKey { index: 4 })
            .testing_insert(finished_cancelling_tasks2);
        cache
            .get(DiceKey { index: 5 })
            .testing_insert(yet_to_cancel_tasks1);
        cache
            .get(DiceKey { index: 6 })
            .testing_insert(yet_to_cancel_tasks2);
        cache
            .get(DiceKey { index: 7 })
            .testing_insert(yet_to_cancel_tasks3);

        let pending_tasks = cache.dupe().cancel_pending_tasks();

        assert_eq!(pending_tasks.len(), 3);

        assert!(matches!(
            cache.get(DiceKey { index: 999 }),
            DiceTaskRef::TransactionCancelled
        ));
    }
}
