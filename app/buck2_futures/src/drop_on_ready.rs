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
use std::task::Context;
use std::task::Poll;

use pin_project::pin_project;

use crate::maybe_future::MaybeFuture;

/// Future that drops the inner future immediately when poll first returns Poll::Ready.
///
/// TODO(cjhopman): It's not clear that this matches expected rust behavior or that it's every actually necessary.
#[pin_project(project = DropOnReadyFutureProj)]
pub(crate) struct DropOnReadyFuture<F>(#[pin] MaybeFuture<F>);

impl<F> DropOnReadyFuture<F> {
    pub(crate) fn new(f: F) -> Self {
        Self(MaybeFuture::Fut(f))
    }
}

impl<F> Future for DropOnReadyFuture<F>
where
    F: Future,
{
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        match this.0.as_mut().poll(cx) {
            Poll::Ready(res) => {
                this.0.take();
                Poll::Ready(res)
            }
            Poll::Pending => Poll::Pending,
        }
    }
}
