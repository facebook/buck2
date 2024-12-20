/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Defines a future with explicit cancellation

mod details;
pub(crate) mod future;

use std::future::Future;
use std::mem::ManuallyDrop;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use dupe::Dupe;
use futures::FutureExt;
use once_cell::sync::Lazy;

use crate::cancellation::details::CancellationContextInner;
use crate::cancellation::details::ExplicitCancellationContext;
use crate::cancellation::details::ExplicitCriticalSectionGuard;
use crate::cancellation::future::context::ExecutionContextInner;
use crate::cancellation::future::CancellationNotificationData;
use crate::cancellation::future::CancellationNotificationFuture;
use crate::details::shared_state::SharedState;

static NEVER_CANCELLED: Lazy<CancellationContext> =
    Lazy::new(|| CancellationContext(CancellationContextInner::NeverCancelled));

/// Context available to the function running inside the future to control and manage it's own
/// cancellation
pub struct CancellationContext(CancellationContextInner);

impl CancellationContext {
    pub fn testing() -> &'static Self {
        &NEVER_CANCELLED
    }

    /// Create a new context from a thread that is never canceled
    pub fn never_cancelled() -> &'static Self {
        &NEVER_CANCELLED
    }

    /// Ignore cancellations while 'CriticalSectionGuard' is held
    pub fn begin_ignore_cancellation(&self) -> CriticalSectionGuard {
        self.0.begin_ignore_cancellation()
    }

    /// Enter a critical section during which the current future (if supports explicit cancellation)
    /// should not be dropped. If the future was not cancelled before entering the critical section,
    /// it becomes non-cancellable during the critical section. If it *was* cancelled before
    /// entering the critical section (i.e. the last ref was dropped during `poll`), then the
    /// future is allowed to continue executing until this future resolves.
    pub fn critical_section<'a, F, Fut>(
        &'a self,
        make: F,
    ) -> impl Future<Output = <Fut as Future>::Output> + 'a
    where
        F: FnOnce() -> Fut + 'a,
        Fut: Future + 'a,
    {
        // placeholder to just delegate to existing critical_section code for now until we migrate
        // the callsites
        self.0.critical_section(make)
    }

    /// Enter a structured cancellation section. The caller receives a CancellationObserver. The
    /// CancellationObserver is a future that resolves when cancellation is requested (or when this
    /// section exits).
    pub fn with_structured_cancellation<'a, F, Fut>(
        &'a self,
        make: F,
    ) -> impl Future<Output = <Fut as Future>::Output> + 'a
    where
        Fut: Future + 'a,
        F: FnOnce(CancellationObserver) -> Fut + 'a,
    {
        self.0.with_structured_cancellation(make)
    }

    /// Tries to ignore the cancellation for this future from here on
    pub fn try_to_keep_going_on_cancellation(&self) -> Option<DisableCancellationGuard> {
        self.begin_ignore_cancellation()
            .keep_going_on_cancellations_if_not_cancelled()
    }

    fn new_explicit(inner: ExecutionContextInner) -> CancellationContext {
        Self(CancellationContextInner::Explicit(
            ExplicitCancellationContext { inner },
        ))
    }
}

/// When held, prevents cancellation of the current explicitly cancellable future.
/// When no longer held (unless drop or cancellations are disabled was not ran), cancellations will be allowed.
/// `disable_cancellations`ing this will leak the guard, causing cancellations to be disabled forever, regardless
/// of whether or not the task is already cancelled.
pub enum CriticalSectionGuard<'a> {
    NeverCancelled,
    Explicit(ExplicitCriticalSectionGuard<'a>),
}

impl<'a> CriticalSectionGuard<'a> {
    pub fn cancellation_observer(&self) -> CancellationObserver {
        match self {
            CriticalSectionGuard::NeverCancelled => {
                CancellationObserver(CancellationObserverInner::NeverCancelled)
            }
            CriticalSectionGuard::Explicit(inner) => inner.cancellation_observer(),
        }
    }

    /// Allow cancellations again, but unlike dropping it, also checks if we should be cancelled
    /// right now, at this specific await point.
    pub async fn allow_cancellations_again(self) {
        match self {
            CriticalSectionGuard::NeverCancelled => {
                // nothing to do.
            }
            CriticalSectionGuard::Explicit(inner) => inner.allow_cancellations_again().await,
        }
    }

    /// Ignores cancellations forever if we are not already cancelled. Otherwise, begin allowing
    /// cancellations again
    pub fn keep_going_on_cancellations_if_not_cancelled(self) -> Option<DisableCancellationGuard> {
        match self {
            CriticalSectionGuard::NeverCancelled => Some(DisableCancellationGuard),
            CriticalSectionGuard::Explicit(inner) => {
                inner.keep_going_on_cancellations_if_not_cancelled()
            }
        }
    }

    fn new_explicit(
        context: &'a ExecutionContextInner,
        notification: CancellationNotificationData,
    ) -> Self {
        Self::Explicit(ExplicitCriticalSectionGuard {
            context: Some(context),
            notification,
        })
    }
}

/// A Future that resolves only when the associated task has been canceled.
#[derive(Clone, Dupe)]
pub struct CancellationObserver(pub(crate) CancellationObserverInner);

impl CancellationObserver {
    pub(crate) fn never_cancelled() -> Self {
        CancellationObserver(CancellationObserverInner::NeverCancelled)
    }
}

#[derive(Clone, Dupe)]
pub(crate) enum CancellationObserverInner {
    NeverCancelled,
    Explicit(CancellationNotificationFuture),
}

impl Future for CancellationObserver {
    type Output = ();

    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        match &mut self.0 {
            CancellationObserverInner::Explicit(fut) => fut.poll_unpin(cx),
            CancellationObserverInner::NeverCancelled => Poll::Pending,
        }
    }
}

/// A marker that indicates that cancellations have been disabled indefinitely for this task.
pub struct DisableCancellationGuard;

/// A handle providing the ability to explicitly cancel the associated ExplicitlyCancellableFuture.
pub struct CancellationHandle {
    shared_state: SharedState,
}

impl CancellationHandle {
    fn new(shared_state: SharedState) -> Self {
        CancellationHandle { shared_state }
    }

    /// Attempts to cancel the future this handle is associated with as soon as possible, returning
    /// a future that completes when the future is canceled.
    pub fn cancel(self) {
        if !self.shared_state.cancel() {
            unreachable!("We consume the CancellationHandle on cancel, so this isn't possible")
        }
    }

    pub fn into_dropcancel(self) -> DropcancelHandle {
        DropcancelHandle::new(self.shared_state)
    }
}

/// A handle that will cancel the associated ExplicitlyCancellableFuture when it is dropped.
pub struct DropcancelHandle {
    shared_state: ManuallyDrop<SharedState>,
}

impl DropcancelHandle {
    fn new(shared_state: SharedState) -> Self {
        Self {
            shared_state: ManuallyDrop::new(shared_state),
        }
    }

    pub fn into_cancellable(mut self) -> CancellationHandle {
        let shared_state = unsafe { ManuallyDrop::take(&mut self.shared_state) };
        std::mem::forget(self);

        CancellationHandle { shared_state }
    }
}

impl Drop for DropcancelHandle {
    fn drop(&mut self) {
        if !self.shared_state.cancel() {
            unreachable!("We consume the handle on cancel, so this isn't possible")
        }
        unsafe { ManuallyDrop::drop(&mut self.shared_state) };
    }
}
