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
use std::marker::PhantomPinned;
use std::mem;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use futures::future::BoxFuture;
use pin_project::pin_project;

use crate::maybe_future::MaybeFuture;

#[pin_project]
pub struct OwningFuture<T, D> {
    // NOTE: The order of these two fields is important. The `OwningFuture` holds a
    // reference to the `context`, and so absolutely must be dropped first.
    #[pin]
    fut: MaybeFuture<BoxFuture<'static, T>>, // not actually static but the lifetime of this struct
    data: D,

    _p: PhantomPinned,
}

impl<T, D> OwningFuture<T, D> {
    pub fn new<F>(data: D, f: F) -> Pin<Box<Self>>
    where
        F: for<'a> FnOnce(&'a mut D) -> BoxFuture<'a, T> + Send,
    {
        let this = OwningFuture {
            data,
            fut: MaybeFuture::None,
            _p: Default::default(),
        };

        let mut this = Box::pin(this);
        let fut = unsafe {
            // SAFETY: The `OwningFuture` is always immediately pinned upon
            // construction, so the self-reference will remain valid at least until this value
            // is dropped. Furthermore, the inner future is dropped before the `context` is dropped,
            // and so the reference will remain valid even during the drop of the `Future`
            mem::transmute::<BoxFuture<'_, T>, BoxFuture<'static, T>>(f(&mut this.data))
        };

        {
            let this = this.as_mut().project();
            this.fut.set_fut(fut);
        }

        this
    }
}

impl<T, D> Future for OwningFuture<T, D> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.project().fut.poll(cx)
    }
}

#[cfg(test)]
mod tests {
    use futures::future::BoxFuture;
    use futures::FutureExt;

    use crate::owning_future::OwningFuture;

    #[tokio::test]
    async fn test_future() {
        struct SomeData(usize);

        fn some_fn(d: &mut SomeData) -> BoxFuture<usize> {
            async move { d.0 }.boxed()
        }

        let data = SomeData(2);
        let fut = OwningFuture::new(data, some_fn);

        // we can spawn the future such that it owns the data
        let data = tokio::spawn(fut).await.unwrap();
        assert_eq!(data, 2);
    }
}
