/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::pin::Pin;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;

use allocative::Allocative;
use bytes::Bytes;
use dupe::Dupe;
use futures::task::Poll;
use futures::Stream;
use pin_project::pin_project;

#[derive(Allocative, Clone, Dupe)]
pub struct HttpNetworkStats {
    pub downloaded_bytes: Arc<AtomicU64>,
}

impl HttpNetworkStats {
    pub fn new() -> Self {
        Self {
            downloaded_bytes: Arc::new(AtomicU64::new(0)),
        }
    }
}

impl HttpNetworkStats {
    pub fn downloaded_bytes(&self) -> &Arc<AtomicU64> {
        &self.downloaded_bytes
    }

    pub fn get_downloaded_bytes(&self) -> u64 {
        self.downloaded_bytes.load(Ordering::Relaxed)
    }
}

#[pin_project]
pub struct CountingStream<S> {
    #[pin]
    inner: S,
    bytes_read: Arc<AtomicU64>,
}

impl<S> CountingStream<S> {
    pub fn new(stream: S, bytes_read: Arc<AtomicU64>) -> Self {
        Self {
            inner: stream,
            bytes_read,
        }
    }
}

impl<S, E> Stream for CountingStream<S>
where
    S: Stream<Item = Result<Bytes, E>>,
{
    type Item = Result<Bytes, E>;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        let mut this = self.project();

        match this.inner.as_mut().poll_next(cx) {
            Poll::Ready(Some(Ok(bytes))) => {
                this.bytes_read
                    .fetch_add(bytes.len() as u64, Ordering::Relaxed);
                Poll::Ready(Some(Ok(bytes)))
            }
            x => x,
        }
    }
}
