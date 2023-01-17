/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;

use futures::stream::FuturesOrdered;
use futures::stream::FuturesUnordered;
use futures::Stream;
use futures::StreamExt;

/// `FuturesOrdered` or `FuturesUnordered`.
// This can be done with GAT, but GAT is unstable, and requires too much work to make it work,
// and this switch has only a little runtime overhead.
pub(crate) enum FuturesQueue<Fut: Future> {
    Ordered(FuturesOrdered<Fut>),
    Unordered(FuturesUnordered<Fut>),
}

impl<Fut: Future> FuturesQueue<Fut> {
    pub(crate) fn new_ordered() -> Self {
        FuturesQueue::Ordered(FuturesOrdered::new())
    }

    pub(crate) fn new_unordered() -> Self {
        FuturesQueue::Unordered(FuturesUnordered::new())
    }

    pub(crate) fn push(&mut self, fut: Fut) {
        match self {
            FuturesQueue::Ordered(futures_ordered) => futures_ordered.push_back(fut),
            FuturesQueue::Unordered(futures_unordered) => futures_unordered.push(fut),
        }
    }
}

impl<Fut: Future> Stream for FuturesQueue<Fut> {
    type Item = Fut::Output;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Self::Item>> {
        match self.get_mut() {
            FuturesQueue::Ordered(futures_ordered) => futures_ordered.poll_next_unpin(cx),
            FuturesQueue::Unordered(futures_unordered) => futures_unordered.poll_next_unpin(cx),
        }
    }
}
