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
pub mod future;

use std::future::Future;

use once_cell::sync::Lazy;

use crate::cancellable_future::CancellationObserver;
use crate::cancellable_future::CancellationObserverInner;
use crate::cancellable_future::DisableCancellationGuard;
use crate::cancellation::details::CancellationContextInner;
use crate::cancellation::details::ExplicitCancellationContext;
use crate::cancellation::details::ExplicitCriticalSectionGuard;
use crate::cancellation::future::context::ExecutionContextInner;
use crate::cancellation::future::CancellationNotificationData;

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
