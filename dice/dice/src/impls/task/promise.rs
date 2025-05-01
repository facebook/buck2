/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Promise is a handle to a DiceTask that will be completed

use std::future::Future;
use std::mem;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use dice_error::result::CancellableResult;
use dupe::Dupe;
use futures::future::BoxFuture;
use futures::task::AtomicWaker;

use crate::arc::Arc;
use crate::impls::task::dice::Cancellations;
use crate::impls::task::dice::DiceTaskInternal;
use crate::impls::task::dice::SlabId;
use crate::impls::task::handle::TaskState;
use crate::impls::value::DiceComputedValue;

/// A string reference to a 'DiceTask' that is pollable as a future.
/// This is only awoken when the result is ready, as none of the pollers are responsible for
/// running the task to completion
///
/// Memory size:
/// DicePromise <-> Shared: DicePromise is 2 triomphe::Arc and a usize, whereas Shared is a usize
/// and 1 std::Arc, so we hold one extra Arc.
pub(crate) struct DicePromise(pub(super) DicePromiseInternal);

pub(super) enum DicePromiseInternal {
    Ready {
        result: DiceComputedValue,
    },
    Pending {
        slab: SlabId,
        task_internal: Arc<DiceTaskInternal>,
        waker: Arc<AtomicWaker>,
        cancellations: Cancellations,
    },
    Done,
}

/// result of a synchronous projection
pub(crate) struct DiceSyncResult {
    /// the value that's ready now without checking core state
    pub(crate) sync_result: DiceComputedValue,
    /// the future value after checking core state
    pub(crate) state_future: BoxFuture<'static, CancellableResult<DiceComputedValue>>,
}

impl DiceSyncResult {
    #[cfg(test)]
    pub(crate) fn testing(v: DiceComputedValue) -> Self {
        use futures::FutureExt;

        Self {
            sync_result: v.dupe(),
            state_future: futures::future::ready(Ok(v)).boxed(),
        }
    }
}

impl DicePromise {
    pub(crate) fn ready(result: DiceComputedValue) -> Self {
        Self(DicePromiseInternal::Ready { result })
    }

    pub(super) fn pending(
        slab: SlabId,
        internal: Arc<DiceTaskInternal>,
        waker: Arc<AtomicWaker>,
        cancellations: Cancellations,
    ) -> Self {
        Self(DicePromiseInternal::Pending {
            slab,
            task_internal: internal,
            waker,
            cancellations,
        })
    }

    pub(crate) fn is_pending(&self) -> bool {
        match &self.0 {
            DicePromiseInternal::Ready { .. } => false,
            DicePromiseInternal::Pending { task_internal, .. } => task_internal.is_pending(),
            DicePromiseInternal::Done => false,
        }
    }

    /// Get the value if already complete, or complete it. Note that `f` may run even if the result
    /// is not used.
    pub(crate) fn sync_get_or_complete(
        self,
        f: impl FnOnce() -> DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        match &self.0 {
            DicePromiseInternal::Ready { result } => Ok(result.dupe()),
            DicePromiseInternal::Pending { task_internal, .. } => {
                if let Some(res) = task_internal.read_value() {
                    res
                } else if let Some(sync_res) = {
                    let lock = task_internal.sync_value.read();
                    let value = lock.dupe();
                    drop(lock);
                    value
                } {
                    Ok(sync_res)
                } else {
                    match task_internal.state.report_project() {
                        TaskState::Continue => {}
                        TaskState::Finished => {
                            return task_internal
                                .read_value()
                                .expect("task finished must mean result is ready");
                        }
                    }

                    let result = {
                        let mut locked = task_internal.sync_value.write();

                        if let Some(res) = locked.as_ref() {
                            return Ok(res.dupe());
                        }

                        let result = f();

                        assert!(
                            locked.replace(result.sync_result.dupe()).is_none(),
                            "should only complete sync result once"
                        );

                        result
                    };

                    tokio::spawn({
                        let future = result.state_future;
                        let internals = task_internal.dupe();

                        async move {
                            let res = future.await;

                            let mut sync_value = internals.sync_value.write();

                            match res {
                                Ok(result) => {
                                    // only errors if cancelled, so we can ignore any errors when
                                    // setting the result
                                    let _ignore = internals.set_value(result);
                                }
                                Err(reason) => {
                                    // if its cancelled, report cancelled
                                    internals.report_terminated(reason);
                                }
                            }

                            // stop storing the sync value since the async one is done
                            sync_value.take()
                        }
                    });

                    Ok(result.sync_result)
                }
            }
            DicePromiseInternal::Done => panic!("poll after ready"),
        }
    }
}

impl Drop for DicePromise {
    fn drop(&mut self) {
        match &self.0 {
            DicePromiseInternal::Ready { .. } => {}
            DicePromiseInternal::Done => {}
            DicePromiseInternal::Pending {
                slab,
                task_internal,
                cancellations,
                ..
            } => task_internal.drop_waiter(slab, cancellations),
        }
    }
}

impl Future for DicePromise {
    type Output = CancellableResult<DiceComputedValue>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let res = match &mut self.0 {
            x @ DicePromiseInternal::Ready { .. } => {
                match mem::replace(x, DicePromiseInternal::Done) {
                    DicePromiseInternal::Ready { result } => Poll::Ready(Ok(result)),
                    _ => unreachable!(),
                }
            }
            DicePromiseInternal::Pending {
                task_internal,
                waker,
                ..
            } => {
                waker.register(cx.waker());
                if let Some(res) = task_internal.read_value() {
                    Poll::Ready(res)
                } else {
                    Poll::Pending
                }
            }
            DicePromiseInternal::Done => panic!("poll after ready"),
        };

        if matches!(res, Poll::Ready(..)) {
            // make sure to set the state to done if we are ready, so that when this task gets
            // dropped later, we do not check the `wakers` mutex
            // The wakers list will have been cleared anyways by the original future that completed
            // the task.
            self.0 = DicePromiseInternal::Done;
        }

        res
    }
}
