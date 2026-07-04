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
use dice_error::result::CancellationReason;
use dupe::Dupe;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::task::dice::DiceTaskRef;
use crate::impls::task::dice::PreparedDiceTask;
use crate::impls::task::dice::ReadValueResult;
use crate::impls::value::DiceComputedValue;

#[derive(Allocative)]
struct Data {
    storage: ShardedLockFreeRawTable<Arc<DiceTaskInternal>, 64>,
    is_cancelled: AtomicBool,
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct SharedCache {
    data: Arc<Data>,
}

pub(crate) enum SharedCacheLookup<'d> {
    Finished(DiceComputedValue),
    InProgress(DiceTaskRef<'d>),
    Vacant,
}

pub(crate) enum SharedCacheInsert<'d> {
    Occupied(DiceTaskRef<'d>),
    Inserted(PreparedDiceTask<'d>),
    TransactionCancelled,
}

impl SharedCache {
    fn key_hash(key: DiceKey) -> u64 {
        (key.index as u64).wrapping_mul(0x9e3779b97f4a7c15)
    }

    pub(crate) fn get(&self, key: DiceKey) -> SharedCacheLookup<'_> {
        let entry = self
            .data
            .storage
            .lookup(Self::key_hash(key), |task| task.key == key);

        match entry {
            Some(task) => {
                if let ReadValueResult::Finished(v) = task.read_value() {
                    SharedCacheLookup::Finished(v.dupe())
                } else {
                    SharedCacheLookup::InProgress(DiceTaskRef { internal: task })
                }
            }
            None => SharedCacheLookup::Vacant,
        }
    }

    pub(crate) fn insert(&self, key: DiceKey) -> SharedCacheInsert {
        if self.data.is_cancelled.load(Ordering::Relaxed) {
            return SharedCacheInsert::TransactionCancelled;
        }

        let maybe_prepared_task = DiceTask::prepare(key, |task| {
            let (entry, not_inserted_value) = self.data.storage.insert(
                Self::key_hash(key),
                task.internal,
                |left, right| left.key == right.key,
                |task| Self::key_hash(task.key),
            );
            let entry = DiceTaskRef { internal: entry };
            match not_inserted_value {
                Some(_) => Err(entry),
                None => Ok(entry),
            }
        });

        if self.data.is_cancelled.load(Ordering::Relaxed) {
            return SharedCacheInsert::TransactionCancelled;
        }

        match maybe_prepared_task {
            Ok(p) => SharedCacheInsert::Inserted(p),
            Err(t) => SharedCacheInsert::Occupied(t),
        }
    }

    #[cfg(test)]
    pub(crate) fn testing_insert_task(&self, key: DiceKey, task: DiceTask) {
        let (_, not_inserted_value) = self.data.storage.insert(
            Self::key_hash(key),
            task.internal,
            |left, right| left.key == right.key,
            |task| Self::key_hash(task.key),
        );
        assert!(not_inserted_value.is_none());
    }

    pub(crate) fn new() -> Self {
        SharedCache {
            data: Arc::new(Data {
                storage: ShardedLockFreeRawTable::new(),
                is_cancelled: AtomicBool::new(false),
            }),
        }
    }

    /// This function gets the termination observer for all running tasks when transaction is
    /// cancelled and prevents further tasks from being added
    pub(crate) fn cancel_pending_tasks(self) -> Vec<DiceTask> {
        self.data.is_cancelled.store(true, Ordering::Relaxed);

        // The pattern with the `is_cancelled` flag is exactly what this is for
        self.data.storage.synchronize_with_inserts();

        self.data
            .storage
            .iter()
            .filter_map(|entry| {
                let task = DiceTaskRef { internal: entry };

                if task.is_pending() {
                    task.cancel(CancellationReason::TransactionDropped);
                    Some(task.clone_arc())
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
    use crate::impls::task::dice::DiceTaskRef;
    use crate::introspection::DiceTaskState;

    impl SharedCache {
        pub(crate) fn iter_tasks(&self) -> impl Iterator<Item = (DiceKey, DiceTaskState)> {
            self.data.storage.iter().map(|entry| {
                (
                    entry.key,
                    DiceTaskRef { internal: entry }.introspect_state(),
                )
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dice_error::result::CancellationReason;
    use dice_futures::cancellation::CancellationContext;
    use dice_futures::spawner::TokioSpawner;
    use dupe::Dupe;
    use futures::FutureExt;
    use pagable::Pagable;
    use pagable::pagable_typetag;

    use crate::DiceKeyDyn;
    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::api::key::NoValueSerialize;
    use crate::api::key::ValueSerialize;
    use crate::impls::cache::SharedCache;
    use crate::impls::cache::SharedCacheInsert;
    use crate::impls::cache::SharedCacheLookup;
    use crate::impls::key::DiceKey;
    use crate::impls::task::dice::DiceTask;
    use crate::impls::task::dice::testing_helpers::make_completed_task;
    use crate::impls::task::spawn_dice_task;

    #[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash, Pagable)]
    #[pagable_typetag(DiceKeyDyn)]
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

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            NoValueSerialize::<Self::Value>::new()
        }
    }

    async fn make_finished_cancelling_task(key: DiceKey) -> DiceTask {
        let finished_cancelling_tasks = spawn_dice_task(key, &TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                futures::future::pending().await
            }
            .boxed()
        });
        finished_cancelling_tasks
            .as_ref()
            .cancel(CancellationReason::ByTest);
        finished_cancelling_tasks.as_ref().await_termination().await;

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

        let completed_key1 = DiceKey { index: 10 };
        let completed_key2 = DiceKey { index: 20 };
        let completed_task1 = make_completed_task::<K>(completed_key1, 1);
        let completed_task2 = make_completed_task::<K>(completed_key2, 2);

        let finished_cancelling_key1 = DiceKey { index: 30 };
        let finished_cancelling_key2 = DiceKey { index: 40 };
        let finished_cancelling_tasks1 =
            make_finished_cancelling_task(finished_cancelling_key1).await;
        let finished_cancelling_tasks2 =
            make_finished_cancelling_task(finished_cancelling_key2).await;

        let pending_key1 = DiceKey { index: 50 };
        let pending_key2 = DiceKey { index: 60 };
        let pending_key3 = DiceKey { index: 70 };
        let yet_to_cancel_tasks1 = make_never_finish_yet_to_cancel_task(pending_key1);
        let yet_to_cancel_tasks2 = make_never_finish_yet_to_cancel_task(pending_key2);
        let yet_to_cancel_tasks3 = make_never_finish_yet_to_cancel_task(pending_key3);

        cache.testing_insert_task(completed_key1, completed_task1);
        cache.testing_insert_task(completed_key2, completed_task2);
        cache.testing_insert_task(finished_cancelling_key1, finished_cancelling_tasks1);
        cache.testing_insert_task(finished_cancelling_key2, finished_cancelling_tasks2);
        cache.testing_insert_task(pending_key1, yet_to_cancel_tasks1);
        cache.testing_insert_task(pending_key2, yet_to_cancel_tasks2);
        cache.testing_insert_task(pending_key3, yet_to_cancel_tasks3);

        assert!(matches!(
            cache.get(completed_key1),
            SharedCacheLookup::Finished(_)
        ));

        let pending_tasks = cache.dupe().cancel_pending_tasks();

        assert_eq!(pending_tasks.len(), 3);
        assert!(matches!(
            cache.insert(DiceKey { index: 999 }),
            SharedCacheInsert::TransactionCancelled
        ));
    }
}
