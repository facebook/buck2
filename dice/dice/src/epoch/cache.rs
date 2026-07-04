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

use std::fmt;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dice_error::DiceError;
use dice_error::DiceResult;
use dupe::Dupe;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

use crate::arc::Arc;
use crate::arc::ArcBorrow;
use crate::epoch::task::dice::DiceTask;
use crate::epoch::task::dice::DiceTaskInternal;
use crate::epoch::task::dice::DiceTaskRef;
use crate::epoch::task::dice::PreparedDiceTask;
use crate::epoch::task::projections::ProjectionTask;
use crate::epoch::task::projections::ProjectionTaskCompletionHandle;
use crate::key::DiceKey;
use crate::value::DiceComputedValue;

#[derive(Debug, Copy, Clone, Dupe)]
pub(crate) struct TransactionCancelled;

/// A `Result`-like wrapper that also represents transaction cancellation.
///
/// This is newtyped because unlike other results, this should almost never be short-circuited. For
/// almost all purposes, a `TransactionCancelled` is treated as a real computation result exactly
/// like a "successful" one.
#[derive(Debug, Clone, Dupe)]
pub(crate) struct TransactionResult<T>(Result<T, TransactionCancelled>);

impl<T> TransactionResult<T> {
    pub(crate) fn ok(t: T) -> Self {
        Self(Ok(t))
    }

    pub(crate) fn err(token: TransactionCancelled) -> Self {
        Self(Err(token))
    }

    pub(crate) const fn make_cancelled() -> Self {
        Self(Err(TransactionCancelled))
    }

    pub(crate) fn unpack(self) -> Result<T, TransactionCancelled> {
        self.0
    }

    pub(crate) fn as_ref(&self) -> TransactionResult<&T> {
        TransactionResult(self.0.as_ref().map_err(|e| *e))
    }

    pub(crate) fn into_dice_result(self) -> DiceResult<T> {
        self.0.map_err(|_| DiceError::transaction_cancelled())
    }
}

#[derive(Allocative)]
struct Data {
    storage: ShardedLockFreeRawTable<Arc<DiceTaskInternal>, 64>,
    projection_storage: ShardedLockFreeRawTable<Arc<ProjectionTask>, 64>,
    is_cancelled: AtomicBool,
}

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct SharedCache {
    data: Arc<Data>,
}

impl fmt::Debug for SharedCache {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("SharedCache")
    }
}

pub(crate) enum SharedCacheLookup<'d, T> {
    Finished(&'d TransactionResult<DiceComputedValue>),
    InProgress(T),
    Vacant,
}

pub(crate) enum SharedCacheInsert<T, N> {
    Occupied(T),
    Inserted(N),
    TransactionCancelled(&'static TransactionResult<DiceComputedValue>),
}

impl<T, N> SharedCacheInsert<T, N> {
    fn cancelled(_t: TransactionCancelled) -> Self {
        static R: TransactionResult<DiceComputedValue> = TransactionResult::make_cancelled();
        Self::TransactionCancelled(&R)
    }
}

impl SharedCache {
    fn key_hash(key: DiceKey) -> u64 {
        (key.index as u64).wrapping_mul(0x9e3779b97f4a7c15)
    }

    pub(crate) fn get(&self, key: DiceKey) -> SharedCacheLookup<'_, DiceTaskRef<'_>> {
        let entry = self
            .data
            .storage
            .lookup(Self::key_hash(key), |task| task.key == key);

        match entry {
            Some(task) => {
                let task = DiceTaskRef { internal: task };
                if let Some(v) = task.get_finished_value() {
                    SharedCacheLookup::Finished(v)
                } else {
                    SharedCacheLookup::InProgress(task)
                }
            }
            None => SharedCacheLookup::Vacant,
        }
    }

    pub(crate) fn get_projection(&self, key: DiceKey) -> SharedCacheLookup<&'_ ProjectionTask> {
        let entry = self
            .data
            .projection_storage
            .lookup(Self::key_hash(key), |task| task.key == key);

        match entry {
            Some(task) => {
                if let Some(v) = task.get().try_read() {
                    SharedCacheLookup::Finished(v)
                } else {
                    SharedCacheLookup::InProgress(task.get())
                }
            }
            None => SharedCacheLookup::Vacant,
        }
    }

    pub(crate) fn insert(
        &self,
        key: DiceKey,
    ) -> SharedCacheInsert<DiceTaskRef<'_>, PreparedDiceTask<'_>> {
        if self.data.is_cancelled.load(Ordering::Relaxed) {
            return SharedCacheInsert::cancelled(TransactionCancelled);
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
            return SharedCacheInsert::cancelled(TransactionCancelled);
        }

        match maybe_prepared_task {
            Ok(p) => SharedCacheInsert::Inserted(p),
            Err(t) => SharedCacheInsert::Occupied(t),
        }
    }

    pub(crate) fn insert_projection(
        &self,
        key: DiceKey,
    ) -> SharedCacheInsert<ArcBorrow<'_, ProjectionTask>, ProjectionTaskCompletionHandle> {
        if self.data.is_cancelled.load(Ordering::Relaxed) {
            return SharedCacheInsert::cancelled(TransactionCancelled);
        }

        let maybe_prepared_task = ProjectionTask::prepare(key, |task| {
            let (entry, not_inserted_value) = self.data.projection_storage.insert(
                Self::key_hash(key),
                task,
                |left, right| left.key == right.key,
                |task| Self::key_hash(task.key),
            );
            if not_inserted_value.is_some() {
                Err(entry)
            } else {
                Ok(entry.get())
            }
        });

        if self.data.is_cancelled.load(Ordering::Relaxed) {
            return SharedCacheInsert::cancelled(TransactionCancelled);
        }

        match maybe_prepared_task {
            Ok(handle) => {
                if self.data.is_cancelled.load(Ordering::Relaxed) {
                    handle.cancel(TransactionCancelled);
                    return SharedCacheInsert::cancelled(TransactionCancelled);
                }
                SharedCacheInsert::Inserted(handle)
            }
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
                projection_storage: ShardedLockFreeRawTable::new(),
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

        let regular = self
            .data
            .storage
            .iter()
            .filter_map(|entry| {
                let task = DiceTaskRef { internal: entry };
                if task.cancel(TransactionCancelled) {
                    Some(task.clone_arc())
                } else {
                    None
                }
            })
            .collect();
        // Projection keys can't really be cancelled; however, our caller is going to await the
        // tasks we return and rely on all ongoing `compute` calls having finished at that time, so
        // we must do something - we take the simple approach of just awaiting all ongoing
        // projections at this point. We know this can't take too long because the number of
        // outstanding projections is bounded by the worker thread count (given their synchronous
        // nature)
        for t in self.data.projection_storage.iter() {
            if t.is_pending() {
                drop(t.wait_sync());
            }
        }

        regular
    }
}

#[cfg(test)]
impl SharedCache {
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }
}

pub(crate) mod introspection {
    use crate::epoch::cache::SharedCache;
    use crate::epoch::task::dice::DiceTaskRef;
    use crate::introspection::DiceTaskState;
    use crate::key::DiceKey;

    impl SharedCache {
        pub(crate) fn iter_tasks(&self) -> impl Iterator<Item = (DiceKey, DiceTaskState)> {
            let regular = self.data.storage.iter().map(|entry| {
                (
                    entry.key,
                    DiceTaskRef { internal: entry }.introspect_state(),
                )
            });
            let projection = self
                .data
                .projection_storage
                .iter()
                .map(|entry| (entry.key, entry.introspect_state()));
            regular.chain(projection)
        }
    }
}

#[cfg(test)]
mod tests {
    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
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
    use crate::epoch::cache::SharedCache;
    use crate::epoch::cache::SharedCacheInsert;
    use crate::epoch::cache::SharedCacheLookup;
    use crate::epoch::cache::TransactionCancelled;
    use crate::epoch::task::dice::DiceTask;
    use crate::epoch::task::dice::testing_helpers::make_completed_task;
    use crate::epoch::task::spawn_dice_task;
    use crate::key::DiceKey;

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
            .cancel(TransactionCancelled);
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
            SharedCacheInsert::TransactionCancelled(_)
        ));
    }
}
