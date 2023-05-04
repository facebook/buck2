/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A future that can be canceled via an explicit `CancellationHandle`.
//! This future is intended to be spawned on tokio-runtime directly, and for its results to be
//! accessed via the joinhandle.
//! It is not intended to be polled directly.
//!

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::future::BoxFuture;
use pin_project::pin_project;

use crate::cancellation::CancellationContext;
use crate::cancellation::CancellationContextInner;

#[allow(unused)] // TODO(temporary)
pub(crate) fn make_future<F, T>(
    f: F,
) -> (
    ExplicitlyCancellableFuture<impl Future<Output = T>>,
    CancellationHandle,
)
where
    F: for<'a> FnOnce(&'a CancellationContext) -> BoxFuture<'a, T>,
{
    let fut = {
        async move {
            let cancel = CancellationContext(CancellationContextInner::Explicit);
            f(&cancel).await
        }
    };

    let fut = ExplicitlyCancellableFuture::new(fut);
    let handle = CancellationHandle;

    (fut, handle)
}

/// Defines a future that operates with the 'CancellationContext' to provide explicit cancellation.
///
/// NOTE: this future is intended only to be polled in a consistent tokio runtime, and never moved
/// from one executor to another.
/// The general safe way of using this future is to spawn it directly via `tokio::spawn`.
#[pin_project(project = ExplicitlyCancellableFutureProj)]
pub struct ExplicitlyCancellableFuture<F> {
    #[pin]
    future: F,
}

impl<F> ExplicitlyCancellableFuture<F>
where
    F: Future,
{
    fn new(future: F) -> Self {
        ExplicitlyCancellableFuture { future }
    }
}

impl<F> ExplicitlyCancellableFutureProj<'_, F>
where
    F: Future,
{
    fn poll_inner(&mut self, cx: &mut Context<'_>) -> Poll<Option<<F as Future>::Output>> {
        self.future.as_mut().poll(cx).map(Some)
    }
}

impl<F> Future for ExplicitlyCancellableFuture<F>
where
    F: Future,
{
    type Output = Option<<F as Future>::Output>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();

        this.poll_inner(cx)
    }
}

pub struct CancellationHandle;

#[cfg(test)]
mod tests {

    use std::task::Poll;

    use assert_matches::assert_matches;
    use futures::FutureExt;

    use crate::cancellation::future::make_future;

    #[tokio::test]
    async fn test_ready() {
        let (fut, _handle) = make_future(|_| futures::future::ready(()).boxed());
        futures::pin_mut!(fut);
        assert_matches!(futures::poll!(fut), Poll::Ready(Some(())));
    }
}
