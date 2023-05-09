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

use futures::task::AtomicWaker;

use crate::api::error::DiceResult;
use crate::arc::Arc;
use crate::impls::task::dice::DiceTaskInternal;
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
        result: DiceResult<DiceComputedValue>,
    },
    Pending {
        slab: usize,
        task_internal: Arc<DiceTaskInternal>,
        waker: Arc<AtomicWaker>,
    },
    Done,
}

impl DicePromise {
    pub(super) fn ready(result: DiceResult<DiceComputedValue>) -> Self {
        Self(DicePromiseInternal::Ready { result })
    }

    pub(super) fn pending(
        slab: usize,
        internal: Arc<DiceTaskInternal>,
        waker: Arc<AtomicWaker>,
    ) -> Self {
        Self(DicePromiseInternal::Pending {
            slab,
            task_internal: internal,
            waker,
        })
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
                ..
            } => task_internal.drop_waiter(*slab),
        }
    }
}

impl Future for DicePromise {
    type Output = DiceResult<DiceComputedValue>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut self.0 {
            x @ DicePromiseInternal::Ready { .. } => {
                match mem::replace(x, DicePromiseInternal::Done) {
                    DicePromiseInternal::Ready { result } => Poll::Ready(result),
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
        }
    }
}
