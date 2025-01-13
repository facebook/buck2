/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;
use std::rc::Rc;

use buck2_common::events::HasEvents;
use buck2_data::BxlDiceInvocationEnd;
use buck2_data::BxlDiceInvocationStart;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_futures::cancellation::CancellationObserver;
use dice::DiceComputations;
use dice::DiceData;
use dice::UserComputationData;
use dupe::Dupe;
use futures::future::select;
use futures::future::Either;
use futures::future::LocalBoxFuture;
use futures::FutureExt;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Tier0)]
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
pub(crate) trait BxlDiceComputations {
    // via() below provides a more useful api for consumers.
    fn via_impl<'a: 'b, 'b>(
        &'a mut self,
        f: Box<
            dyn for<'d> FnOnce(
                    &'a mut DiceComputations<'d>,
                ) -> LocalBoxFuture<'a, buck2_error::Result<()>>
                + 'b,
        >,
    ) -> buck2_error::Result<()>;

    fn global_data(&self) -> &DiceData;

    fn per_transaction_data(&self) -> &UserComputationData;
}

impl dyn BxlDiceComputations + '_ {
    // We require that BxlDiceComputations be object-safe, but that means we can't have a type parameter in `via_impl`.
    // It's really inconvenient to not have that, though, so we provide an implementation here that supports it.
    pub(crate) fn via<'a, T: 'a>(
        &'a mut self,
        // The returned future as a 'a lifetime to allow people to capture things in the future with a matching lifetime to self.
        f: impl for<'d> FnOnce(
            &'a mut DiceComputations<'d>,
        ) -> LocalBoxFuture<'a, buck2_error::Result<T>>
        + 'a,
    ) -> buck2_error::Result<T> {
        // We can't capture a &mut res here in the closure unfortunately, so we need to do this little dance to get values out.
        let res: Rc<OnceCell<T>> = Rc::new(OnceCell::new());
        let res2 = res.clone();
        self.via_impl(Box::new(move |dice| {
            async move {
                res2.set(f(dice).await?).ok().unwrap();
                Ok(())
            }
            .boxed_local()
        }))?;
        Ok(Rc::try_unwrap(res).ok().unwrap().take().unwrap())
    }
}

impl BxlDiceComputations for BxlSafeDiceComputations<'_, '_> {
    fn via_impl<'a: 'b, 'b>(
        &'a mut self,
        f: Box<
            dyn for<'d> FnOnce(
                    &'a mut DiceComputations<'d>,
                ) -> LocalBoxFuture<'a, buck2_error::Result<()>>
                + 'b,
        >,
    ) -> buck2_error::Result<()> {
        let dispatcher = self.0.per_transaction_data().get_dispatcher().dupe();

        dispatcher.span(BxlDiceInvocationStart {}, || {
            let liveness = self.1.dupe();
            let fut = with_dispatcher_async(dispatcher.clone(), async move { f(self.0).await });
            let fut = async move {
                futures::pin_mut!(fut);

                match select(fut, liveness).await {
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

    fn global_data(&self) -> &DiceData {
        self.0.global_data()
    }

    fn per_transaction_data(&self) -> &UserComputationData {
        self.0.per_transaction_data()
    }
}

pub(crate) struct BxlSafeDiceComputations<'a, 'd>(
    &'a mut DiceComputations<'d>,
    CancellationObserver,
);

impl<'a, 'd> BxlSafeDiceComputations<'a, 'd> {
    pub(crate) fn new(
        dice: &'a mut DiceComputations<'d>,
        cancellation: CancellationObserver,
    ) -> Self {
        Self(dice, cancellation)
    }
}
