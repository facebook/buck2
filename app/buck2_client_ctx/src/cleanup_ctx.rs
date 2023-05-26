/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::Mutex;
use std::time::Duration;
use std::time::Instant;

use dupe::Dupe;
use futures::future;
use futures::future::BoxFuture;
use tokio::runtime::Runtime;

/// For cleanup we want to perform, but cant do in `drop` because it's async.
#[derive(Clone, Dupe)]
pub struct AsyncCleanupContext<'a> {
    jobs: Arc<Mutex<Vec<BoxFuture<'static, ()>>>>,
    runtime: &'a Runtime,
}

impl<'a> AsyncCleanupContext<'a> {
    pub fn register(&self, name: &'static str, fut: BoxFuture<'static, ()>) {
        const WARNING_TIMEOUT: Duration = Duration::from_millis(1000);
        self.jobs
            .lock()
            .expect("Poisoned mutex")
            .push(Box::pin(async move {
                let start = Instant::now();
                fut.await;
                let elapsed = start.elapsed();
                if elapsed > WARNING_TIMEOUT {
                    tracing::warn!("Async cleanup step \'{}\' took {:?}", name, elapsed);
                } else {
                    tracing::info!("Async cleanup step \'{}\' took {:?}", name, elapsed);
                };
            }));
    }

    async fn join(&self) {
        let futs = std::mem::take(&mut *self.jobs.lock().expect("Poisoned mutex"));
        future::join_all(futs).await;
    }
}

pub struct AsyncCleanupContextGuard<'a>(AsyncCleanupContext<'a>);

impl<'a> AsyncCleanupContextGuard<'a> {
    pub fn new(runtime: &'a Runtime) -> Self {
        Self(AsyncCleanupContext {
            jobs: Arc::new(Mutex::new(Vec::new())),
            runtime,
        })
    }

    pub fn ctx(&self) -> &AsyncCleanupContext<'a> {
        &self.0
    }
}

impl<'a> Drop for AsyncCleanupContextGuard<'a> {
    fn drop(&mut self) {
        let future = self.0.join();
        self.ctx().runtime.block_on(async move {
            if tokio::time::timeout(Duration::from_secs(30), future)
                .await
                .is_err()
            {
                tracing::warn!("Timeout waiting for async cleanup");
            }
        });
    }
}
