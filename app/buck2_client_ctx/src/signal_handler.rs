/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use futures::Future;
use futures::future;
use futures::future::Either;

/// A simple SIGINT handler that lets `work` and ctrl+c future race. When ctrl+c
/// is hit, it allows the `work` future and the other clean-up implementations
/// such as AsyncCleanupContext to be dropped.
pub async fn with_simple_sigint_handler<F: Future>(work: F) -> Option<F::Output> {
    let exit = tokio::signal::ctrl_c();

    futures::pin_mut!(work);
    futures::pin_mut!(exit);

    match future::select(work, exit).await {
        Either::Left((res, _)) => Some(res),
        Either::Right((_, _)) => None,
    }
}
