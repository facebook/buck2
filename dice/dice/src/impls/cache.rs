/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Shared, concurrent dice task cache that is shared between computations at the same version

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use dashmap::mapref::entry::Entry;
use dashmap::DashMap;
use dupe::Dupe;
use fnv::FnvBuildHasher;
use more_futures::cancellation::future::TerminationObserver;

use crate::arc::Arc;
use crate::impls::key::DiceKey;
use crate::impls::task::dice::DiceTask;

#[derive(Allocative, Clone, Dupe)]
pub(crate) struct SharedCache {
    data: Arc<Data>,
}

#[derive(Allocative)]
struct Data {
    storage: DashMap<DiceKey, DiceTask, FnvBuildHasher>,
    is_cancelled: AtomicBool,
}

impl SharedCache {
    pub(crate) fn get(&self, key: DiceKey) -> Option<Entry<DiceKey, DiceTask, FnvBuildHasher>> {
        let entry = self.data.storage.entry(key);
        if self.data.is_cancelled.load(Ordering::Acquire) {
            None
        } else {
            Some(entry)
        }
    }

    pub(crate) fn new() -> Self {
        Self {
            data: Arc::new(Data {
                storage: DashMap::default(),
                is_cancelled: AtomicBool::new(false),
            }),
        }
    }

    pub(crate) fn active_tasks_count(&self) -> usize {
        self.data.storage.len()
    }

    #[allow(unused)] // temporary
    /// This function gets the termination observer for all running tasks when transaction is
    /// cancelled and prevents further tasks from being added
    pub(crate) fn cancel_pending_tasks(self) -> Vec<TerminationObserver> {
        self.data.is_cancelled.store(true, Ordering::Release);
        self.data
            .storage
            .iter()
            .filter_map(|entry| {
                if entry.value().is_pending() {
                    entry.value().cancel()
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

#[cfg(test)]
mod tests {
    use std::any::Any;

    use allocative::Allocative;
    use async_trait::async_trait;
    use derive_more::Display;
    use dupe::Dupe;
    use futures::FutureExt;
    use more_futures::cancellation::CancellationContext;
    use more_futures::spawner::TokioSpawner;

    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::arc::Arc;
    use crate::impls::cache::SharedCache;
    use crate::impls::core::graph::history::CellHistory;
    use crate::impls::key::DiceKey;
    use crate::impls::key::ParentKey;
    use crate::impls::task::dice::DiceTask;
    use crate::impls::task::spawn_dice_task;
    use crate::impls::value::DiceComputedValue;
    use crate::impls::value::DiceKeyValue;
    use crate::impls::value::DiceValidValue;
    use crate::impls::value::MaybeValidDiceValue;

    #[derive(Allocative, Clone, Debug, Display, Eq, PartialEq, Hash)]
    struct K;

    #[async_trait]
    impl Key for K {
        type Value = usize;

        async fn compute(
            &self,
            _ctx: &DiceComputations,
            _cancellations: &CancellationContext,
        ) -> Self::Value {
            unimplemented!("test")
        }

        fn equality(_: &Self::Value, _: &Self::Value) -> bool {
            true
        }
    }

    async fn make_completed_task(val: usize) -> DiceTask {
        let task = spawn_dice_task(&TokioSpawner, &(), |handle| {
            async move {
                handle.finished(Ok(DiceComputedValue::new(
                    MaybeValidDiceValue::valid(DiceValidValue::testing_new(
                        DiceKeyValue::<K>::new(val),
                    )),
                    Arc::new(CellHistory::empty()),
                )));

                Box::new(()) as Box<dyn Any + Send>
            }
            .boxed()
        });

        task.depended_on_by(ParentKey::None)
            .not_cancelled()
            .unwrap()
            .await
            .unwrap();

        task
    }

    async fn make_finished_cancelling_task() -> DiceTask {
        let finished_cancelling_tasks = spawn_dice_task(&TokioSpawner, &(), |handle| {
            async move {
                let _handle = handle;
                futures::future::pending().await
            }
            .boxed()
        });
        finished_cancelling_tasks.cancel().unwrap().await;

        finished_cancelling_tasks
    }

    fn make_never_finish_yet_to_cancel_task() -> DiceTask {
        spawn_dice_task(&TokioSpawner, &(), |handle| {
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

        let completed_task1 = make_completed_task(1).await;
        let completed_task2 = make_completed_task(2).await;

        let finished_cancelling_tasks1 = make_finished_cancelling_task().await;
        let finished_cancelling_tasks2 = make_finished_cancelling_task().await;

        let yet_to_cancel_tasks1 = make_never_finish_yet_to_cancel_task();
        let yet_to_cancel_tasks2 = make_never_finish_yet_to_cancel_task();
        let yet_to_cancel_tasks3 = make_never_finish_yet_to_cancel_task();

        cache
            .get(DiceKey { index: 1 })
            .unwrap()
            .or_insert(completed_task1);
        cache
            .get(DiceKey { index: 2 })
            .unwrap()
            .or_insert(completed_task2);
        cache
            .get(DiceKey { index: 3 })
            .unwrap()
            .or_insert(finished_cancelling_tasks1);
        cache
            .get(DiceKey { index: 4 })
            .unwrap()
            .or_insert(finished_cancelling_tasks2);
        cache
            .get(DiceKey { index: 5 })
            .unwrap()
            .or_insert(yet_to_cancel_tasks1);
        cache
            .get(DiceKey { index: 6 })
            .unwrap()
            .or_insert(yet_to_cancel_tasks2);
        cache
            .get(DiceKey { index: 7 })
            .unwrap()
            .or_insert(yet_to_cancel_tasks3);

        let pending_tasks = cache.dupe().cancel_pending_tasks();

        assert_eq!(pending_tasks.len(), 3);

        assert!(cache.get(DiceKey { index: 999 }).is_none());
    }
}
