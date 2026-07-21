/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Promise is a handle to a DiceTask that will be completed

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use dice_error::result::CancellableResult;
use pin_project::pin_project;

use crate::epoch::task::dice::DiceTaskDependentFuture;
use crate::value::DiceComputedValue;

/// A strong reference to a 'DiceTask' that is pollable as a future.
/// This is only awoken when the result is ready, as none of the pollers are responsible for
/// running the task to completion.
#[pin_project]
pub(crate) struct DicePromise<'d>(#[pin] pub(super) DicePromiseInternal<'d>);

#[pin_project(project = DicePromiseInternalProj)]
pub(super) enum DicePromiseInternal<'d> {
    Ready {
        result: CancellableResult<&'d DiceComputedValue>,
    },
    Pending {
        #[pin]
        future: DiceTaskDependentFuture<'d>,
    },
    Done,
}

impl<'d> DicePromise<'d> {
    pub(crate) fn ready(result: CancellableResult<&'d DiceComputedValue>) -> Self {
        Self(DicePromiseInternal::Ready { result })
    }

    pub(crate) fn pending(future: DiceTaskDependentFuture<'d>) -> Self {
        Self(DicePromiseInternal::Pending { future })
    }
}

impl<'d> Future for DicePromise<'d> {
    type Output = CancellableResult<&'d DiceComputedValue>;

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let res = match self.as_mut().project().0.project() {
            DicePromiseInternalProj::Ready { result } => Poll::Ready(*result),
            DicePromiseInternalProj::Pending { future } => future.poll(cx),
            DicePromiseInternalProj::Done => panic!("poll after ready"),
        };

        if matches!(res, Poll::Ready(..)) {
            self.project().0.set(DicePromiseInternal::Done);
        }

        res
    }
}
