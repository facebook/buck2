/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Futures with specialized dropping behaviour

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::Stream;
use pin_project::pin_project;

/// A future with a value attached that will be dropped together when the main future is dropped.
#[pin_project]
pub struct DropTogether<F, D> {
    #[pin]
    fut: F,
    // we don't really care about this. Anything is droppable.
    _drop_with: D,
}

impl<F, D> DropTogether<F, D> {
    pub fn new(fut: F, t: D) -> Self {
        Self { fut, _drop_with: t }
    }
}

impl<F, D> Future for DropTogether<F, D>
where
    F: Future,
{
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let this = self.project();
        this.fut.poll(cx)
    }
}

impl<S, D> Stream for DropTogether<S, D>
where
    S: Stream,
{
    type Item = S::Item;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        this.fut.poll_next(cx)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use futures::future;
    use futures::stream::FuturesOrdered;
    use futures::StreamExt;
    use gazebo::prelude::*;

    use crate::drop::DropTogether;

    #[test]
    fn drops_together() {
        let ref_counted = Arc::new(());

        let fut = async {};
        let drop_together = DropTogether::new(fut, ref_counted.dupe());

        assert_eq!(Arc::strong_count(&ref_counted), 2);

        drop(drop_together);
        assert_eq!(Arc::strong_count(&ref_counted), 1);
    }

    #[test]
    fn pollable() {
        let fut = async { 1 };
        let drop_together = DropTogether::new(fut, ());

        assert_eq!(futures::executor::block_on(drop_together), 1);
    }

    #[test]
    fn streamable() {
        let fut = {
            let mut fut = FuturesOrdered::new();
            fut.push_back(future::ready(1));
            fut.push_back(future::ready(2));
            fut
        };
        let mut drop_together = DropTogether::new(fut, ());

        futures::executor::block_on(async move {
            assert_eq!(Some(1), drop_together.next().await);
            assert_eq!(Some(2), drop_together.next().await);
        });
    }
}
