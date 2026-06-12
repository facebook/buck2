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
use pin_project::pin_project;

use crate::impls::task::dice::DiceTaskDependentFuture;
use crate::impls::value::DiceComputedValue;

/// A strong reference to a 'DiceTask' that is pollable as a future.
/// This is only awoken when the result is ready, as none of the pollers are responsible for
/// running the task to completion.
#[pin_project]
pub(crate) struct DicePromise(#[pin] pub(super) DicePromiseInternal);

#[pin_project(project = DicePromiseInternalProj)]
pub(super) enum DicePromiseInternal {
    Ready {
        result: DiceComputedValue,
    },
    Pending {
        #[pin]
        future: DiceTaskDependentFuture,
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

    pub(crate) fn pending(future: DiceTaskDependentFuture) -> Self {
        Self(DicePromiseInternal::Pending { future })
    }

    /// Get the value if already complete, or complete it. Note that `f` may run even if the result
    /// is not used.
    pub(crate) fn sync_get_or_complete(
        self,
        f: impl FnOnce() -> DiceSyncResult,
    ) -> CancellableResult<DiceComputedValue> {
        match self.0 {
            DicePromiseInternal::Ready { result } => Ok(result),
            DicePromiseInternal::Pending { future, .. } => {
                future.task().dupe().sync_get_or_complete(future, f)
            }
            DicePromiseInternal::Done => panic!("poll after ready"),
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
            DicePromiseInternal::Done => panic!("poll after ready"),
            _ => match self.as_mut().project().0.project() {
                DicePromiseInternalProj::Pending { future } => match future.poll(cx) {
                    Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
                    v => v,
                },
                _ => unreachable!(),
            },
        };

        if matches!(res, Poll::Ready(..)) {
            self.0 = DicePromiseInternal::Done;
        }

        res
    }
}
