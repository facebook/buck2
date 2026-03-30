/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::pin::Pin;
use std::sync::Arc;
use std::task::Context;
use std::task::Poll;

use buck2_error::internal_error;
use buck2_util::threads::thread_spawn;
use dupe::Dupe;
use futures::stream::Stream;
use futures::stream::StreamExt;
use tokio::sync::OwnedSemaphorePermit;
use tokio::sync::Semaphore;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Priority {
    High,
    Low,
}

struct AcquireReq {
    permits: u32,
    grant_tx: oneshot::Sender<PriorityPermit>,
}

/// A permit returned by PrioritySemaphore.
/// Dropping the permit releases the permits back to the semaphore.
#[derive(Debug)]
pub struct PriorityPermit {
    _inner: OwnedSemaphorePermit,
}

#[derive(Clone)]
pub struct PrioritySemaphore {
    inner: Arc<Inner>,
}

struct Inner {
    high_tx: mpsc::UnboundedSender<AcquireReq>,
    low_tx: mpsc::UnboundedSender<AcquireReq>,
}

/// A stream that yields acquire requests in priority order: high before low.
struct PriorityStream {
    high_rx: mpsc::UnboundedReceiver<AcquireReq>,
    low_rx: mpsc::UnboundedReceiver<AcquireReq>,
}

impl Stream for PriorityStream {
    type Item = AcquireReq;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.get_mut();

        let high_closed = match this.high_rx.poll_recv(cx) {
            Poll::Ready(Some(req)) => return Poll::Ready(Some(req)),
            Poll::Ready(None) => true,
            Poll::Pending => false,
        };

        match this.low_rx.poll_recv(cx) {
            Poll::Ready(Some(req)) => return Poll::Ready(Some(req)),
            Poll::Ready(None) if high_closed => return Poll::Ready(None),
            _ => {}
        }

        Poll::Pending
    }
}

impl PrioritySemaphore {
    pub fn new(thread_name: &str, permits: usize) -> Self {
        let (high_tx, high_rx) = mpsc::unbounded_channel::<AcquireReq>();
        let (low_tx, low_rx) = mpsc::unbounded_channel::<AcquireReq>();

        let semaphore = Arc::new(Semaphore::new(permits));

        let stream = PriorityStream { high_rx, low_rx };

        let _handle = thread_spawn(thread_name, move || {
            let rt = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            rt.block_on(run_manager(stream, semaphore));
        });

        Self {
            inner: Arc::new(Inner { high_tx, low_tx }),
        }
    }

    pub async fn acquire(&self, priority: Priority) -> buck2_error::Result<PriorityPermit> {
        self.acquire_many(1, priority).await
    }

    pub async fn acquire_many(
        &self,
        permits: u32,
        priority: Priority,
    ) -> buck2_error::Result<PriorityPermit> {
        let (grant_tx, grant_rx) = oneshot::channel();

        let request = AcquireReq { permits, grant_tx };

        let tx = match priority {
            Priority::High => &self.inner.high_tx,
            Priority::Low => &self.inner.low_tx,
        };

        tx.send(request)
            .map_err(|_| internal_error!("priority semaphore manager task is closed"))?;
        grant_rx
            .await
            .map_err(|_| internal_error!("priority semaphore manager task is closed"))
    }
}

async fn run_manager(mut stream: PriorityStream, semaphore: Arc<Semaphore>) {
    while let Some(req) = stream.next().await {
        // Skip cancelled requests
        if req.grant_tx.is_closed() {
            continue;
        }

        // Acquire permits from the semaphore, blocking until available.
        // This preserves priority ordering: high priority requests are pulled from
        // the stream before low priority ones, and we wait for permits before
        // pulling the next request.
        match semaphore.dupe().acquire_many_owned(req.permits).await {
            Ok(permit) => {
                drop(req.grant_tx.send(PriorityPermit { _inner: permit }));
            }
            Err(_) => {
                // Semaphore was closed
                break;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use tokio::sync::mpsc;
    use tokio::time::timeout;

    use super::Priority;
    use super::PrioritySemaphore;

    #[tokio::test]
    async fn high_priority_served_before_low() {
        let sem = PrioritySemaphore::new("test", 1);
        // Hold the only permit
        let hold = sem.acquire(Priority::High).await.unwrap();

        // Block the manager on the semaphore so subsequent requests queue untouched
        let sem_blocker = sem.clone();
        let _blocker = tokio::spawn(async move {
            let _permit = sem_blocker.acquire(Priority::High).await.unwrap();
        });
        tokio::time::sleep(Duration::from_millis(20)).await;

        let (order_tx, mut order_rx) = mpsc::unbounded_channel();

        // Queue 5 Low requests
        for i in 0..5 {
            let sem_i = sem.clone();
            let tx = order_tx.clone();
            tokio::spawn(async move {
                let _permit = sem_i.acquire(Priority::Low).await.unwrap();
                tx.send(format!("low-{}", i)).unwrap();
            });
        }

        // Queue 5 High requests
        for i in 0..5 {
            let sem_i = sem.clone();
            let tx = order_tx.clone();
            tokio::spawn(async move {
                let _permit = sem_i.acquire(Priority::High).await.unwrap();
                tx.send(format!("high-{}", i)).unwrap();
            });
        }

        // Let all requests land in their channels
        tokio::time::sleep(Duration::from_millis(20)).await;
        drop(order_tx);

        // Release. Manager finishes blocker, then drains high before low.
        drop(hold);

        let mut results = Vec::new();
        while let Ok(Some(val)) = timeout(Duration::from_secs(2), order_rx.recv()).await {
            results.push(val);
        }

        assert_eq!(results.len(), 10);
        assert!(results[..5].iter().all(|s| s.starts_with("high")));
        assert!(results[5..].iter().all(|s| s.starts_with("low")));
    }

    #[tokio::test]
    async fn lazy_cancellation_does_not_block_acquires() {
        let sem = PrioritySemaphore::new("test", 1);
        let hold = sem.acquire(Priority::High).await.unwrap();

        // Queue 3 acquires that we'll cancel
        let mut to_cancel = Vec::new();
        for _ in 0..3 {
            let sem_i = sem.clone();
            to_cancel.push(tokio::spawn(async move {
                let _permit = sem_i.acquire(Priority::High).await.unwrap();
            }));
        }
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Queue the real waiter *before* cancelling — it sits behind the 3 in the channel
        let sem_next = sem.clone();
        let next = tokio::spawn(async move {
            sem_next.acquire(Priority::High).await.unwrap();
        });
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Cancel the 3 — their oneshot channels close, manager will skip them
        for task in to_cancel {
            task.abort();
        }

        // Release permit. If cancelled requests blocked, this would timeout.
        drop(hold);
        timeout(Duration::from_secs(1), next)
            .await
            .unwrap()
            .unwrap();
    }
}
