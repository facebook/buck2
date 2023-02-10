/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;

use buck2_common::events::HasEvents;
use buck2_events::dispatch::with_dispatcher_async;
use dice::DiceComputations;
use dupe::Dupe;
use futures::future::select;
use futures::future::Either;
use more_futures::cancellable_future::CancellationObserver;
use thiserror::Error;

#[derive(Error, Debug)]
enum ViaError {
    #[error("The owning DICE evaluation has been cancelled")]
    Cancelled,
}

/// Provides a safe blocking calls to async functions for starlark that requires operations to
/// be not async.
///
/// This is not exposed to starlark but rather, used by operations exposed to starlark to run
/// code.
/// This also provides a handle for dice.
pub struct BxlSafeDiceComputations<'a>(pub(crate) &'a DiceComputations, &'a CancellationObserver);

impl<'a> BxlSafeDiceComputations<'a> {
    pub fn new(dice: &'a DiceComputations, cancellation: &'a CancellationObserver) -> Self {
        Self(dice, cancellation)
    }

    /// runs the async computation over dice as sync
    pub fn via_dice<Fut, T>(&self, f: impl FnOnce(&'a DiceComputations) -> Fut) -> anyhow::Result<T>
    where
        Fut: Future<Output = anyhow::Result<T>>,
    {
        self.via(|| f(self.0))
    }

    /// runs any async computation
    pub fn via<Fut, T>(&self, f: impl FnOnce() -> Fut) -> anyhow::Result<T>
    where
        Fut: Future<Output = anyhow::Result<T>>,
    {
        let dispatcher = self.0.per_transaction_data().get_dispatcher().dupe();
        let fut = with_dispatcher_async(dispatcher, f());
        let fut = async move {
            futures::pin_mut!(fut);

            match select(fut, self.1.dupe()).await {
                Either::Left((res, _)) => res,
                Either::Right(((), _)) => Err(ViaError::Cancelled.into()),
            }
        };

        tokio::runtime::Handle::current().block_on(fut)
    }
}
