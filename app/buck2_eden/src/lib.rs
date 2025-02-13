/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

// This can't be built in our OSS implementation.
#![cfg(fbcode_build)]
#![feature(error_generic_member_access)]
#![feature(used_with_arg)]

pub mod connection;
pub mod error;
pub mod io_provider;

pub mod semaphore {
    use buck2_core::buck2_env;
    use tokio::sync::Semaphore;

    // This value was selected semi-randomly and should be revisited in the future. Anecdotally, we
    // have seen EdenFS struggle with <<< 2048 outstanding requests, but the exact number depends
    // on the size/complexity/cost of the outstanding requests.
    pub static DEFAULT_MAX_OUTSTANDING_REQUESTS: usize = 2048;

    /// A default semaphore that is used to limit the number of outstanding requests to EdenFS.
    pub fn default() -> Semaphore {
        Semaphore::new(DEFAULT_MAX_OUTSTANDING_REQUESTS)
    }

    /// A buck2-specific semaphore that is used to limit the number of outstanding requests to
    /// EdenFS. Reads buck2 specific environment variable "BUCK2_EDEN_SEMAPHORE" to determine the
    /// number of permits.
    pub fn buck2_default() -> Semaphore {
        let permits =
            buck2_env!("BUCK2_EDEN_SEMAPHORE", type=usize, default=DEFAULT_MAX_OUTSTANDING_REQUESTS, applicability=internal)
                .unwrap_or(DEFAULT_MAX_OUTSTANDING_REQUESTS);
        Semaphore::new(permits)
    }
}
