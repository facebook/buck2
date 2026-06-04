/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
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
use crate::impls::task::dice::DiceTask;
use crate::impls::task::dice::SlabId;
use crate::impls::value::DiceComputedValue;

/// A strong reference to a 'DiceTask' that is pollable as a future.
/// This is only awoken when the result is ready, as none of the pollers are responsible for
/// running the task to completion.
pub(crate) struct DicePromise(pub(super) DicePromiseInternal);

pub(super) enum DicePromiseInternal {
    Ready {
        result: DiceComputedValue,
    },
    Pending {
        slab: SlabId,
        task: DiceTask,
        waker: Arc<AtomicWaker>,
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

    pub(super) fn pending(slab: SlabId, task: DiceTask, waker: Arc<AtomicWaker>) -> Self {
        Self(DicePromiseInternal::Pending { slab, task, waker })
    }

    pub(crate) fn is_pending(&self) -> bool {
        match &self.0 {
            DicePromiseInternal::Ready { .. } => false,
            DicePromiseInternal::Pending { task, .. } => task.is_pending(),
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
            DicePromiseInternal::Pending { task, .. } => task.sync_get_or_complete(f),
            DicePromiseInternal::Done => panic!("poll after ready"),
        }
    }
}

impl Drop for DicePromise {
    fn drop(&mut self) {
        match &self.0 {
            DicePromiseInternal::Ready { .. } => {}
            DicePromiseInternal::Done => {}
            DicePromiseInternal::Pending { slab, task, .. } => task.drop_waiter(slab),
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
            DicePromiseInternal::Pending { task, waker, .. } => {
                waker.register(cx.waker());
                if let Some(res) = task.read_value() {
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
