/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::OnceLock;

use allocative::Allocative;
use dice_error::result::CancellableResult;
use dice_error::result::CancellationReason;
use dupe::Dupe;
use futures::future::BoxFuture;
use parking_lot::MappedRwLockReadGuard;
use parking_lot::RwLock;
use parking_lot::RwLockReadGuard;

use crate::arc::Arc;
use crate::introspection::DiceTaskState;
use crate::key::DiceKey;
use crate::value::DiceComputedValue;

/// The result of actually evaluating the `compute` on a projection
pub(crate) struct DiceSyncResult {
    /// The value that's ready now without checking core state
    ///
    /// Usually, after finishing a `Key::compute` and before letting rdeps use the result, we
    /// roundtrip to the core state. The value returned after that is typically about the same as
    /// the value we had before, but it may be valid at a wider range of versions.
    ///
    /// On sync computes we can't go to the core state and so we kind of make up a conservative
    /// version range and spawn a background task that'll report the real one later. This is the
    /// value that we use until then.
    pub(crate) sync_result: DiceComputedValue,
    /// The future with the value that core state returns
    pub(crate) state_future: BoxFuture<'static, CancellableResult<DiceComputedValue>>,
}

impl DiceSyncResult {
    #[cfg(test)]
    pub(crate) fn testing(v: DiceComputedValue) -> Self {
        use dupe::Dupe;
        use futures::FutureExt;

        Self {
            sync_result: v.dupe(),
            state_future: futures::future::ready(Ok(v)).boxed(),
        }
    }
}

#[derive(Allocative)]
#[allocative(skip)]
pub(crate) struct ProjectionTask {
    pub(crate) key: DiceKey,
    /// This holds the sync result as described above when it's ready.
    ///
    /// An important note about the semantics here: For basically all purposes, we treat this task
    /// as having "completed" once the sync result is available here and before the core state
    /// result comes back. That's anyway the point at which rdeps can begin using this value, so the
    /// opposite would be very difficult to reason about.
    critical: RwLock<Option<Box<Critical>>>,
    /// The value that the core state returned, once it's actually ready.
    ///
    /// This is always set before the `Some` in `critical` is replaced with a `None`.
    value: OnceLock<CancellableResult<DiceComputedValue>>,
}

struct Critical {
    sync_value: OnceLock<CancellableResult<DiceComputedValue>>,
}

/// The handle given to the thread responsible for performing and completing the computation.
///
/// This type should be treated as linear - the caller absolutely must take the handle and either
/// `complete` or `cancel` it.
// Note: Without real linear types, this is a bit difficult to enforce and so we prevent accidents
// by providing a `Drop` impl. FIXME(JakobDegen): If we were more confident in cancellation testing
// we wouldn't need this.
pub(crate) struct ProjectionTaskCompletionHandle(Option<Arc<ProjectionTask>>);

impl ProjectionTask {
    #[cfg(test)]
    pub(crate) fn prepare_testing(
        key: DiceKey,
    ) -> (Arc<ProjectionTask>, ProjectionTaskCompletionHandle) {
        let mut t = None;
        let h = Self::prepare::<()>(key, |task| {
            t = Some(task.dupe());
            Ok(Box::leak(Box::new(task)).as_ref())
        })
        .unwrap();
        (t.unwrap(), h)
    }

    /// Prepare a new projection task
    ///
    /// This would not need the callback API, but it's kept for consistency with the non-projection
    /// analogue.
    pub(crate) fn prepare<'d, E>(
        key: DiceKey,
        alloc: impl FnOnce(Arc<ProjectionTask>) -> Result<&'d ProjectionTask, E>,
    ) -> Result<ProjectionTaskCompletionHandle, E> {
        let t = Arc::new(Self {
            key,
            critical: RwLock::new(Some(Box::new(Critical {
                sync_value: OnceLock::new(),
            }))),
            value: OnceLock::new(),
        });
        // We don't actually need the return value but we keep it for consistency with the
        // non-projection API
        _ = alloc(t.dupe())?;
        Ok(ProjectionTaskCompletionHandle(Some(t)))
    }

    /// Read the finished value if it's available
    pub(crate) fn try_read(&self) -> Option<CancellableResult<&'_ DiceComputedValue>> {
        self.value.get().map(|r| r.as_ref().map_err(|c| *c))
    }

    fn value_or_guard(
        &self,
    ) -> Result<&CancellableResult<DiceComputedValue>, MappedRwLockReadGuard<'_, Critical>> {
        // Fast path check
        if let Some(v) = self.value.get() {
            return Ok(v);
        }
        let critical = self.critical.read();
        if let Ok(r) = RwLockReadGuard::try_map(critical, |c| c.as_deref()) {
            Err(r)
        } else {
            // We raced, `value` must have been set at this point though
            Ok(self.value.get().expect("Value must have been set"))
        }
    }

    fn insert_computed(
        this: Arc<Self>,
        result: DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        match this.value_or_guard() {
            Ok(v) => {
                // This doesn't normally happen, except in the case of a cancellation
                return v.dupe();
            }
            Err(g) => {
                if g.sync_value.set(Ok(result.sync_result.dupe())).is_err() {
                    // Same here, someone else beat us to it. For consistency, make sure we return that value
                    return g.sync_value.get().unwrap().dupe();
                }
            }
        }
        // We do not in any meaningful way attempt to keep track of and in particular cancel this
        // task. That's justified by our knowledge of what `state_future` does, but not by much
        // else.
        tokio::spawn({
            let future = result.state_future;
            async move {
                let Ok(res) = future.await else {
                    // FIXME(JakobDegen): It's really not clear what we should do in the
                    // cancellation case. However, in the face of uncertainty it's probably best to
                    // guarantee that `.try_read()` on a projection task always agree on whether
                    // things were cancelled or not, so we ignore the cancellation.
                    return;
                };
                this.value.set(Ok(res)).unwrap();
                // Clear `critical` now that we're done
                drop(this.critical.write().take().unwrap());
            }
        });

        Ok(result.sync_result)
    }

    fn cancel(&self, reason: CancellationReason) {
        // This is effectively exactly the same logic as `insert_computed`
        if let Err(g) = self.value_or_guard() {
            if g.sync_value.set(Err(reason)).is_ok() {
                self.value.set(Err(reason)).unwrap();
                drop(g);
                drop(self.critical.write().take().unwrap());
            }
        }
    }

    pub(crate) fn wait_sync(&self) -> CancellableResult<DiceComputedValue> {
        match self.value_or_guard() {
            Ok(v) => v.dupe(),
            Err(g) => g.sync_value.wait().dupe(),
        }
    }

    pub(crate) fn is_pending(&self) -> bool {
        match self.value_or_guard() {
            Ok(_) => false,
            Err(g) => g.sync_value.get().is_none(),
        }
    }

    pub(crate) fn introspect_state(&self) -> DiceTaskState {
        if self.is_pending() {
            DiceTaskState::Ready
        } else {
            DiceTaskState::InProgress
        }
    }
}

impl ProjectionTaskCompletionHandle {
    pub(crate) fn complete(
        mut self,
        result: DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        ProjectionTask::insert_computed(self.0.take().unwrap(), result)
    }

    pub(crate) fn cancel(mut self, reason: CancellationReason) {
        self.0.take().unwrap().cancel(reason);
    }
}

impl Drop for ProjectionTaskCompletionHandle {
    fn drop(&mut self) {
        if let Some(t) = self.0.take() {
            // Attempt to enforce that this handle was completed or cancelled. Cancellation paths
            // tend to be a bit poorly tested though, so do that in unit tests only.
            #[cfg(not(test))]
            t.cancel(CancellationReason::HandleDropped);
            #[cfg(test)]
            {
                drop(t);
                unreachable!();
            }
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
    use dupe::Dupe;
    use futures::FutureExt;
    use pagable::Pagable;
    use pagable::pagable_typetag;
    use tokio::sync::oneshot;

    use super::ProjectionTask;
    use crate::DiceKeyDyn;
    use crate::api::computations::DiceComputations;
    use crate::api::key::Key;
    use crate::api::key::NoValueSerialize;
    use crate::api::key::ValueSerialize;
    use crate::arc::Arc;
    use crate::impls::task::projections::DiceSyncResult;
    use crate::key::DiceKey;
    use crate::value::DiceComputedValue;
    use crate::value::DiceKeyValue;
    use crate::value::DiceValidValue;
    use crate::value::MaybeValidDiceValue;
    use crate::value::TrackedInvalidationPaths;
    use crate::versions::VersionRanges;

    #[derive(Allocative, Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Pagable)]
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

        fn equality(x: &Self::Value, y: &Self::Value) -> bool {
            x == y
        }

        fn value_serialize() -> impl ValueSerialize<Value = Self::Value> {
            NoValueSerialize::<Self::Value>::new()
        }
    }

    fn computed(val: usize) -> DiceComputedValue {
        DiceComputedValue::new(
            MaybeValidDiceValue::valid(DiceValidValue::testing_new(DiceKeyValue::<K>::new(val))),
            Arc::new(VersionRanges::new()),
            TrackedInvalidationPaths::clean(),
        )
    }

    fn is_val(v: &DiceComputedValue, val: usize) -> bool {
        v.value()
            .equality(&DiceValidValue::testing_new(DiceKeyValue::<K>::new(val)))
    }

    /// The winner of the insertion computes the value once; that value is handed back to it and to
    /// everyone else waiting on the same projection.
    #[tokio::test]
    async fn insert_computed_delivers_value_to_winner_and_waiters() -> anyhow::Result<()> {
        let (task, handle) = ProjectionTask::prepare_testing(DiceKey { index: 100 });

        // Callers that ask for the value before it has been computed block until it lands.
        let waiters: Vec<_> = (0..3)
            .map(|_| {
                let task = task.dupe();
                tokio::task::spawn_blocking(move || task.wait_sync())
            })
            .collect();
        // Give the waiters a chance to reach their blocking point before the value is inserted.
        tokio::task::yield_now().await;

        let returned = handle.complete(DiceSyncResult::testing(computed(2)));
        assert!(
            is_val(&returned?, 2),
            "insert_computed returns the computed value"
        );

        for waiter in waiters {
            assert!(
                is_val(&waiter.await??, 2),
                "every waiter observes the computed value"
            );
        }

        Ok(())
    }

    /// `insert_computed` exposes the conservative sync value immediately, then the spawned
    /// background task replaces it with the value reported by core state.
    #[tokio::test]
    async fn async_future_replaces_sync_value() -> anyhow::Result<()> {
        let (task, handle) = ProjectionTask::prepare_testing(DiceKey { index: 100 });

        // `2` is the sync value; `99` is the (deliberately different) value the future reports.
        let (tx, rx) = oneshot::channel();
        let returned = handle.complete(DiceSyncResult {
            sync_result: computed(2),
            state_future: rx
                .map(|res| res.map_err(|_| CancellationReason::ByTest).flatten())
                .boxed(),
        });

        assert!(
            is_val(&returned.as_ref().unwrap(), 2),
            "the sync value is returned immediately"
        );
        assert!(
            task.try_read().is_none(),
            "the final value is unavailable until the future resolves"
        );
        assert!(
            is_val(&task.wait_sync()?, 2),
            "waiters get the sync value while the future is pending"
        );

        tx.send(Ok(computed(99))).unwrap();
        while task.try_read().is_none() {
            tokio::task::yield_now().await;
        }

        assert!(
            is_val(task.try_read().unwrap()?, 99),
            "the final value reflects the future's result"
        );
        assert!(
            is_val(&task.wait_sync()?, 99),
            "wait_sync now fast-paths to the final value"
        );

        Ok(())
    }
}
