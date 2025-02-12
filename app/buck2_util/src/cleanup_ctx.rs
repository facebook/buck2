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

/// For cleanup we want to perform, but cant do in `drop` because it's async.
#[derive(Clone, Dupe)]
pub struct AsyncCleanupContext {
    jobs: Arc<Mutex<Vec<BoxFuture<'static, ()>>>>,
}

impl AsyncCleanupContext {
    pub fn new() -> Self {
        Self {
            jobs: Arc::new(Mutex::new(Vec::new())),
        }
    }

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

    pub async fn cleanup(&self) {
        let futs = std::mem::take(&mut *self.jobs.lock().expect("Poisoned mutex"));
        if tokio::time::timeout(Duration::from_secs(30), future::join_all(futs))
            .await
            .is_err()
        {
            tracing::warn!("Timeout waiting for async cleanup");
        }
    }
}
