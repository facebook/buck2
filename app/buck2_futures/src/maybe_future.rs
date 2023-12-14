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

/// Future that might or might not be there
#[pin_project(project = MaybeFutureProj)]
pub(crate) enum MaybeFuture<F> {
    Fut(#[pin] F),
    None,
}

impl<F: Future> MaybeFuture<F> {
    pub(crate) fn take(mut self: Pin<&mut Self>) {
        self.set(MaybeFuture::None);
    }

    pub(crate) fn set_fut(mut self: Pin<&mut Self>, f: F) {
        self.set(MaybeFuture::Fut(f));
    }
}

impl<F> Future for MaybeFuture<F>
where
    F: Future,
{
    type Output = F::Output;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match self.project() {
            MaybeFutureProj::Fut(f) => f.poll(cx),
            MaybeFutureProj::None => {
                panic!("polled again after completion")
            }
        }
    }
}
