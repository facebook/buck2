/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;

use dupe::Dupe;
use futures::future::Future;
use futures::future::FutureExt;
use futures::future::Shared;
use futures::task::Context;
use futures::task::Poll;
use tokio::sync::oneshot;

use crate::cancellation::future::CancellationNotificationFuture;

#[derive(Clone, Default)]
pub struct CancellationObserver(pub(crate) CancellationObserverInner);

impl CancellationObserver {
    pub(crate) fn never_cancelled() -> Self {
        CancellationObserver(CancellationObserverInner::NeverCancelled)
    }
}

#[derive(Clone)]
pub(crate) enum CancellationObserverInner {
    NeverCancelled,
    Legacy(Option<Shared<oneshot::Receiver<()>>>),
    Explicit(CancellationNotificationFuture),
}

impl Default for CancellationObserverInner {
    fn default() -> Self {
        CancellationObserverInner::Legacy(Default::default())
    }
}

impl Dupe for CancellationObserver {}

impl Future for CancellationObserver {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut self.0 {
            CancellationObserverInner::Legacy(fut) => match fut {
                Some(ref mut rx) => rx.poll_unpin(cx).map(|_| ()),
                None => Poll::Pending,
            },
            CancellationObserverInner::Explicit(fut) => fut.poll_unpin(cx),
            CancellationObserverInner::NeverCancelled => Poll::Pending,
        }
    }
}

/// A marker that indicates that cancellations have been disabled indefinitely for this task.
pub struct DisableCancellationGuard;
