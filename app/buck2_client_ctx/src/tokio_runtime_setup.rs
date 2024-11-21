/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_error::BuckErrorContext;
use buck2_util::tokio_runtime::new_tokio_runtime;

/// Tokio runtime used by the client commands.
pub fn client_tokio_runtime() -> buck2_error::Result<tokio::runtime::Runtime> {
    // Do not use current thread because current thread may have too low thread size.
    new_tokio_runtime("buck2-cli")
        // Tokio creates this number of threads,
        // and creating too many threads for short commands is expensive.
        .worker_threads(1)
        .enable_all()
        .build()
        .buck_error_context("Building tokio runtime")
}
