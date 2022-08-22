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

use futures::future;
use futures::future::BoxFuture;
use gazebo::dupe::Dupe;
use tokio::runtime::Builder;

/// For cleanup we want to perform, but cant do in `drop` because it's async.
#[derive(Clone, Dupe)]
pub(crate) struct AsyncCleanupContext {
    jobs: Arc<Mutex<Vec<BoxFuture<'static, ()>>>>,
}

impl AsyncCleanupContext {
    pub(crate) fn register(&self, name: &'static str, fut: BoxFuture<'static, ()>) {
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

pub(crate) struct AsyncCleanupContextGuard(AsyncCleanupContext);

impl AsyncCleanupContextGuard {
    pub fn new() -> Self {
        Self(AsyncCleanupContext {
            jobs: Arc::new(Mutex::new(Vec::new())),
        })
    }

    pub fn ctx(&self) -> &AsyncCleanupContext {
        &self.0
    }
}

impl Drop for AsyncCleanupContextGuard {
    fn drop(&mut self) {
        let runtime = Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Should be able to start a runtime");
        runtime.block_on(self.0.join());
    }
}
