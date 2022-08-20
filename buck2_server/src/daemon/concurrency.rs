//! Handles command concurrency.
//!
//! `buck2` supports limited concurrency for commands.
//! If there are no buckconfig changes, nor file changes, then commands can be allowed to execute
//! concurrently. Otherwise, `buck2` will block waiting for other commands to finish.

#![allow(clippy::extra_unused_lifetimes)] // FIXME?

use std::future::Future;
use std::sync::Arc;

use async_condvar_fair::BatonExt;
use async_condvar_fair::Condvar;
use buck2_events::TraceId;
use dice::DiceTransaction;
use gazebo::prelude::*;
use parking_lot::FairMutex;
use starlark::collections::SmallMap;

/// Manages concurrent commands, blocking when appropriate.
///
/// Currently, we allow concurrency if two `DiceTransactions` are deemed equivalent, such that
/// any computation result that occurs in one is directly reusable by another.
#[derive(Clone, Dupe)]
pub struct ConcurrencyHandler {
    data: Arc<FairMutex<ConcurrencyHandlerData>>,
    // use an async condvar because the `wait` to `notify` spans across an async function (namely
    // the entire command execution). Luckily, this implementation is also "fair", waking up the
    // oldest waiting command.
    cond: Arc<Condvar>,
}

struct ConcurrencyHandlerData {
    // the currently active `Dice` being used. Commands can only run concurrently if these are
    // "equivalent".
    active_dice: Option<DiceTransaction>,
    // A list of the currently running traces. It's theoretically possible that we use the same
    // trait twice if we support user supplied `TraceId` and have nested invocations, so we keep
    // a map of number of occurrences.
    active_traces: SmallMap<TraceId, usize>,
}

#[allow(unused)] // TODO(bobyf) temporary
impl ConcurrencyHandler {
    pub fn new() -> Self {
        ConcurrencyHandler {
            data: Arc::new(FairMutex::new(ConcurrencyHandlerData {
                active_dice: None,
                active_traces: SmallMap::new(),
            })),
            cond: Default::default(),
        }
    }

    /// Enters a critical section that requires concurrent command synchronization,
    /// and runs the given `exec` function in the critical section.
    pub async fn enter<F, Fut, R>(&self, transaction: DiceTransaction, trace: TraceId, exec: F) -> R
    where
        F: FnOnce(DiceTransaction) -> Fut,
        Fut: Future<Output = R> + Send + 'static,
    {
        let (_guard, transaction) = self.wait_for_others(transaction, trace).await;

        exec(transaction).await
    }

    #[allow(clippy::await_holding_lock)]
    // this is normally super unsafe, but because we are using an async condvar that takes care
    // of unlocking this mutex, this mutex is actually essentially never held across awaits.
    // The async condvar will handle properly allowing under threads to proceed, avoiding
    // starvation.
    async fn wait_for_others(
        &self,
        transaction: DiceTransaction,
        trace: TraceId,
    ) -> (OnExecExit, DiceTransaction) {
        let mut data = self.data.lock();
        let mut baton = None;

        loop {
            if let Some(active_dice) = &data.active_dice {
                if active_dice.equivalent(&transaction) {
                    // if the dice context is equivalent, then we can run concurrently with the
                    // current command.

                    *data.active_traces.entry(trace.dupe()).or_default() += 1;

                    break;
                } else {
                    (data, baton) = self.cond.wait_baton(data).await;
                }
            } else {
                data.active_dice = Some(transaction.dupe());
                *data.active_traces.entry(trace.dupe()).or_default() += 1;

                break;
            }
        }

        // free the lock. We are done managing the command semaphore state such that we
        // are now registered to be running.
        drop(data);

        // create the on exit drop handler, which will take care of notifying tasks.
        // this lets us dispose of the `Baton`, which is no longer necessary for
        // ensuring that on exit/cancellation, the `notify` is passed onto another
        // thread. (the drop of `OnExit` will take care of it).
        let on_exit = OnExecExit(self.dupe(), trace);
        baton.dispose();

        (on_exit, transaction)
    }
}

/// Held to execute a command so that when the command is canceled, we properly remove its state
/// from the handler so that it's no longer registered as a ongoing command.
struct OnExecExit(ConcurrencyHandler, TraceId);

impl<'a> Drop for OnExecExit {
    fn drop(&mut self) {
        let mut data = self.0.data.lock();
        let refs = {
            let refs = data
                .active_traces
                .get_mut(&self.1)
                .expect("command was active but not in active traces");
            *refs -= 1;

            *refs
        };
        if refs == 0 {
            data.active_traces.remove(&self.1);
        }

        if data.active_traces.is_empty() {
            data.active_dice.take();

            // condvar is fair. This will notifying the longest waiting command.
            self.0.cond.notify_one()
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    use async_trait::async_trait;
    use buck2_events::TraceId;
    use derive_more::Display;
    use dice::cycles::DetectCycles;
    use dice::Dice;
    use dice::DiceComputations;
    use dice::Key;
    use gazebo::prelude::*;
    use tokio::sync::Barrier;
    use tokio::sync::RwLock;

    use crate::daemon::concurrency::ConcurrencyHandler;

    #[tokio::test]
    async fn concurrent_same_transaction() {
        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new();

        let traces1 = TraceId::new();
        let traces2 = TraceId::new();
        let traces3 = TraceId::new();

        let ctx1 = dice.ctx();
        let ctx2 = dice.ctx();
        let ctx3 = dice.ctx();

        let barrier = Arc::new(Barrier::new(3));

        let fut1 = concurrency.enter(ctx1, traces1, |_| {
            let b = barrier.dupe();
            async move {
                b.wait().await;
            }
        });
        let fut2 = concurrency.enter(ctx2, traces2, |_| {
            let b = barrier.dupe();
            async move {
                b.wait().await;
            }
        });
        let fut3 = concurrency.enter(ctx3, traces3, |_| {
            let b = barrier.dupe();
            async move {
                b.wait().await;
            }
        });

        futures::future::join3(fut1, fut2, fut3).await;
    }

    #[tokio::test]
    async fn different_traceid_blocks() -> anyhow::Result<()> {
        #[derive(Clone, Dupe, Display, Debug, Hash, Eq, PartialEq)]
        struct K;

        #[async_trait]
        impl Key for K {
            type Value = ();

            async fn compute(&self, _ctx: &DiceComputations) -> Self::Value {
                unimplemented!()
            }

            fn equality(_x: &Self::Value, _y: &Self::Value) -> bool {
                false
            }
        }

        let dice = Dice::builder().build(DetectCycles::Enabled);

        let concurrency = ConcurrencyHandler::new();

        let traces1 = TraceId::new();
        let traces2 = traces1.dupe();
        let traces_different = TraceId::new();

        let ctx1 = dice.ctx();
        let ctx2 = dice.ctx();

        let ctx_different = {
            let ctx = dice.ctx();
            ctx.changed(vec![K])?;
            anyhow::Ok(ctx.commit())
        }?;

        let block1 = Arc::new(RwLock::new(()));
        let blocked1 = block1.write().await;

        let block2 = Arc::new(RwLock::new(()));
        let blocked2 = block2.write().await;

        let barrier1 = Arc::new(Barrier::new(3));
        let barrier2 = Arc::new(Barrier::new(2));

        let arrived = Arc::new(AtomicBool::new(false));

        let fut1 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block1.dupe();

            async move {
                concurrency
                    .enter(ctx1, traces1, |_| async move {
                        barrier.wait().await;
                        let _g = b.read().await;
                    })
                    .await
            }
        });

        let fut2 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier1.dupe();
            let b = block2.dupe();

            async move {
                concurrency
                    .enter(ctx2, traces2, |_| async move {
                        barrier.wait().await;
                        let _g = b.read().await;
                    })
                    .await
            }
        });

        barrier1.wait().await;

        let fut3 = tokio::spawn({
            let concurrency = concurrency.dupe();
            let barrier = barrier2.dupe();
            let arrived = arrived.dupe();

            async move {
                barrier.wait().await;
                concurrency
                    .enter(ctx_different, traces_different, |_| async move {
                        arrived.store(true, Ordering::Relaxed);
                    })
                    .await
            }
        });

        barrier2.wait().await;

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked1);
        fut1.await.unwrap();

        assert!(!arrived.load(Ordering::Relaxed));

        drop(blocked2);
        fut2.await.unwrap();

        fut3.await.unwrap();

        assert!(arrived.load(Ordering::Relaxed));

        Ok(())
    }
}
