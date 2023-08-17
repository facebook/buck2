/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::ops::Deref;

use buck2_common::events::HasEvents;
use buck2_data::BxlDiceInvocationEnd;
use buck2_data::BxlDiceInvocationStart;
use buck2_events::dispatch::with_dispatcher_async;
use dice::DiceComputations;
use dice::DiceData;
use dupe::Dupe;
use futures::future::select;
use futures::future::Either;
use futures::future::LocalBoxFuture;
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
#[derive(Clone, Dupe)]
pub struct BxlSafeDiceComputations<'a>(pub(super) &'a DiceComputations, CancellationObserver);

/// For a `via_dice`, the DiceComputations provided to each lambda is a reference that's only
/// available for some specific lifetime `'x`. This is express as a higher rank lifetime bound
/// `for <'x>` in rust. However, `for <'x>` bounds do not have constraints on them so rust infers
/// them to be any lifetime, including 'static, which is wrong. So, we introduce an extra lifetime
/// here which forces rust compiler to infer additional bounds on the `for <'x>` as a
/// `&'x DiceComputationRef<'a>` cannot live more than `'a`, so using this type as the argument
/// to the closure forces the correct lifetime bounds to be inferred by rust.
pub struct DiceComputationsRef<'s>(&'s DiceComputations);

impl<'s> Deref for DiceComputationsRef<'s> {
    type Target = DiceComputations;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a> BxlSafeDiceComputations<'a> {
    pub fn new(dice: &'a DiceComputations, cancellation: CancellationObserver) -> Self {
        Self(dice, cancellation)
    }

    /// runs any async computation
    pub(super) fn via<'s, T>(
        &'s self,
        f: impl for<'x> FnOnce(&'x DiceComputationsRef<'s>) -> LocalBoxFuture<'x, anyhow::Result<T>>,
    ) -> anyhow::Result<T>
    where
        'a: 's,
    {
        let dispatcher = self.0.per_transaction_data().get_dispatcher().dupe();

        dispatcher.span(BxlDiceInvocationStart {}, || {
            let fut = with_dispatcher_async(dispatcher.clone(), async move {
                let ctx = DiceComputationsRef(self.0);
                f(&ctx).await
            });
            let fut = async move {
                futures::pin_mut!(fut);

                match select(fut, self.1.dupe()).await {
                    Either::Left((res, _)) => res,
                    Either::Right(((), _)) => Err(ViaError::Cancelled.into()),
                }
            };

            (
                tokio::runtime::Handle::current().block_on(fut),
                BxlDiceInvocationEnd {},
            )
        })
    }

    pub fn global_data(&self) -> &DiceData {
        self.0.global_data()
    }
}
