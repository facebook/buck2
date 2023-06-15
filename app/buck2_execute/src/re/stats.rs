/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;

use allocative::Allocative;
use futures::FutureExt;

pub struct RemoteExecutionClientOpStats {
    pub started: u32,
    pub finished_successfully: u32,
    pub finished_with_error: u32,
}

impl From<&'_ OpStats> for RemoteExecutionClientOpStats {
    fn from(stats: &OpStats) -> RemoteExecutionClientOpStats {
        RemoteExecutionClientOpStats {
            started: stats.started.load(Ordering::Relaxed),
            finished_successfully: stats.finished_successfully.load(Ordering::Relaxed),
            finished_with_error: stats.finished_with_error.load(Ordering::Relaxed),
        }
    }
}

pub struct RemoteExecutionClientStats {
    /// In bytes.
    pub uploaded: u64,
    /// In bytes.
    pub downloaded: u64,
    pub uploads: RemoteExecutionClientOpStats,
    pub downloads: RemoteExecutionClientOpStats,
    pub action_cache: RemoteExecutionClientOpStats,
    pub executes: RemoteExecutionClientOpStats,
    pub materializes: RemoteExecutionClientOpStats,
    pub write_action_results: RemoteExecutionClientOpStats,
    pub get_digest_expirations: RemoteExecutionClientOpStats,
}

#[derive(Default, Allocative)]
pub(super) struct OpStats {
    started: AtomicU32,
    finished_successfully: AtomicU32,
    finished_with_error: AtomicU32,
}

impl OpStats {
    pub(super) fn op<'a, R, F>(&'a self, f: F) -> impl Future<Output = anyhow::Result<R>> + 'a
    where
        F: Future<Output = anyhow::Result<R>> + 'a,
    {
        // We avoid using `async fn` or `async move` here to avoid doubling the
        // future size. See https://github.com/rust-lang/rust/issues/62958
        self.started.fetch_add(1, Ordering::Relaxed);
        f.map(|result| {
            (if result.is_ok() {
                &self.finished_successfully
            } else {
                &self.finished_with_error
            })
            .fetch_add(1, Ordering::Relaxed);
            result
        })
    }
}
