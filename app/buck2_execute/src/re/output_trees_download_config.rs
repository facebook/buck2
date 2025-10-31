/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use dupe::Dupe;
use tokio::sync::Semaphore;

pub struct OutputTreesDownloadSemaphore {
    pub semaphore: Semaphore,
    pub max_concurrent_bytes: u32,
}

/// Configuration for controlling concurrent output trees downloads.
#[derive(Clone, Dupe)]
pub struct OutputTreesDownloadConfig {
    semaphore: Option<Arc<OutputTreesDownloadSemaphore>>,
    fingerprint_re_output_trees_eagerly: bool,
}

impl OutputTreesDownloadConfig {
    pub fn new(
        max_concurrent_bytes: Option<u32>,
        fingerprint_re_output_trees_eagerly: bool,
    ) -> Self {
        Self {
            semaphore: max_concurrent_bytes.map(|size| {
                Arc::new(OutputTreesDownloadSemaphore {
                    semaphore: Semaphore::new(size.try_into().unwrap()), // We expect a u32 to fit in a usize.
                    max_concurrent_bytes: size,
                })
            }),
            fingerprint_re_output_trees_eagerly,
        }
    }

    pub fn semaphore(&self) -> Option<&OutputTreesDownloadSemaphore> {
        self.semaphore.as_ref().map(|s| s.as_ref())
    }

    pub fn fingerprint_re_output_trees_eagerly(&self) -> bool {
        self.fingerprint_re_output_trees_eagerly
    }
}
