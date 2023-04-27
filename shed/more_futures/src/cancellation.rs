/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Defines a future with explicit cancellation

use std::future::Future;

use once_cell::sync::Lazy;

use crate::cancellable_future::critical_section;
use crate::cancellable_future::try_to_disable_cancellation;
use crate::cancellable_future::with_structured_cancellation;
use crate::cancellable_future::CancellationObserver;
use crate::cancellable_future::DisableCancellationGuard;

static INSTANCE: Lazy<CancellationContext> =
    Lazy::new(|| CancellationContext(CancellationContextInner::ThreadLocal));

/// Context available to the function running inside the future to control and manage it's own
/// cancellation
pub struct CancellationContext(CancellationContextInner);

impl CancellationContext {
    /// TODO replace with real initialization
    pub fn todo() -> Self {
        CancellationContext(CancellationContextInner::ThreadLocal)
    }

    pub fn testing<'a>() -> &'a Self {
        &INSTANCE
    }

    /// Create a new context from a thread that is never canceled
    pub fn never_cancelled() -> &'static Self {
        &INSTANCE
    }

    /// Enter a critical section during which the current future (if supports explicit cancellation)
    /// should not be dropped. If the future was not cancelled before entering the critical section,
    /// it becomes non-cancellable during the critical section. If it *was* cancelled before
    /// entering the critical section (i.e. the last ref was dropped during `poll`), then the
    /// future is allowed to continue executing until this future resolves.
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
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
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
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

    /// Obtain a StrongRefCount for the current task. This will return None if the task *is* within a
    /// CancellableFuture but has already been cancelled.
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
    pub fn try_to_disable_cancellation(&self) -> Option<DisableCancellationGuard> {
        self.0.try_to_disable_cancellation()
    }
}

enum CancellationContextInner {
    /// The old thread local based implementation
    ThreadLocal,
    /// The cancellation context passed explicitly
    #[allow(unused)]
    Explicit,
}

impl CancellationContextInner {
    /// Enter a critical section during which the current future (if supports explicit cancellation)
    /// should not be dropped. If the future was not cancelled before entering the critical section,
    /// it becomes non-cancellable during the critical section. If it *was* cancelled before
    /// entering the critical section (i.e. the last ref was dropped during `poll`), then the
    /// future is allowed to continue executing until this future resolves.
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
    pub fn critical_section<'a, F, Fut>(
        &'a self,
        make: F,
    ) -> impl Future<Output = <Fut as Future>::Output> + 'a
    where
        F: FnOnce() -> Fut + 'a,
        Fut: Future + 'a,
    {
        match self {
            CancellationContextInner::ThreadLocal => critical_section(make),
            CancellationContextInner::Explicit => {
                unimplemented!("todo")
            }
        }
    }

    /// Enter a structured cancellation section. The caller receives a CancellationObserver. The
    /// CancellationObserver is a future that resolves when cancellation is requested (or when this
    /// section exits).
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
    pub fn with_structured_cancellation<'a, F, Fut>(
        &'a self,
        make: F,
    ) -> impl Future<Output = <Fut as Future>::Output> + 'a
    where
        Fut: Future + 'a,
        F: FnOnce(CancellationObserver) -> Fut + 'a,
    {
        match self {
            CancellationContextInner::ThreadLocal => with_structured_cancellation(make),
            CancellationContextInner::Explicit => {
                unimplemented!("todo")
            }
        }
    }

    /// Obtain a StrongRefCount for the current task. This will return None if the task *is* within a
    /// CancellableFuture but has already been cancelled.
    ///
    /// TODO(bobyf) this needs to be updated with the new implementation
    pub fn try_to_disable_cancellation(&self) -> Option<DisableCancellationGuard> {
        match self {
            CancellationContextInner::ThreadLocal => try_to_disable_cancellation(),
            CancellationContextInner::Explicit => {
                unimplemented!("todo")
            }
        }
    }
}
