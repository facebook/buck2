/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::OnceCell;
use std::rc::Rc;

use buck2_common::events::HasEvents;
use buck2_data::BxlDiceInvocationEnd;
use buck2_data::BxlDiceInvocationStart;
use buck2_events::dispatch::with_dispatcher_async;
use dice::DiceComputations;
use dice::DiceData;
use dice::UserComputationData;
use dice_futures::cancellation::CancellationObserver;
use dupe::Dupe;
use futures::FutureExt;
use futures::future::Either;
use futures::future::LocalBoxFuture;
use futures::future::select;

use crate::bxl::starlark_defs::context::lifetime_erase::LifetimeErased;
use crate::bxl::starlark_defs::context::lifetime_erase::LifetimeErasedTypeClass;

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
pub(crate) trait BxlDiceComputations<'s> {
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

    /// Avoid using if at all possible; use `via_impl` instead
    fn with_inner_less_safe<'a: 'b, 'b>(
        &'a mut self,
        f: Box<dyn for<'d> FnOnce(&'a mut DiceComputations<'d>) + 'b>,
    );
}

impl<'s> dyn BxlDiceComputations<'s> + '_ {
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

impl<'s> BxlDiceComputations<'s> for BxlSafeDiceComputations<'s> {
    fn via_impl<'a: 'b, 'b>(
        &'a mut self,
        f: Box<
            dyn for<'d> FnOnce(
                    &'a mut DiceComputations<'d>,
                ) -> LocalBoxFuture<'a, buck2_error::Result<()>>
                + 'b,
        >,
    ) -> buck2_error::Result<()> {
        let dispatcher = self
            .0
            .access_mut(|dice| dice.per_transaction_data().get_dispatcher().dupe());

        dispatcher.span(BxlDiceInvocationStart {}, || {
            let liveness = self.1.dupe();
            let fut = with_dispatcher_async(dispatcher.clone(), self.0.access_mut(f));
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

    fn global_data<'a>(&'a self) -> &'a DiceData {
        self.0.access(|dice| dice.global_data())
    }

    fn per_transaction_data(&self) -> &UserComputationData {
        self.0.access(|dice| dice.per_transaction_data())
    }

    fn with_inner_less_safe<'a: 'b, 'b>(
        &'a mut self,
        f: Box<dyn for<'d> FnOnce(&'a mut DiceComputations<'d>) + 'b>,
    ) {
        self.0.access_mut(f)
    }
}

struct DiceComputationsEraseTypeClass;

impl LifetimeErasedTypeClass for DiceComputationsEraseTypeClass {
    type Concrete<'d> = DiceComputations<'d>;
}

pub(crate) struct BxlSafeDiceComputations<'a>(
    LifetimeErased<'a, DiceComputationsEraseTypeClass>,
    CancellationObserver,
);

impl<'a> BxlSafeDiceComputations<'a> {
    pub(crate) fn new<'d>(
        dice: &'a mut DiceComputations<'d>,
        cancellation: CancellationObserver,
    ) -> Self {
        Self(LifetimeErased::new(dice), cancellation)
    }
}
