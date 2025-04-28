/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use dupe::Dupe;
use futures::future::Either;

use crate::cancellation::CancellationObserver;
use crate::cancellation::CancellationObserverInner;
use crate::cancellation::CriticalSectionGuard;
use crate::cancellation::DisableCancellationGuard;
use crate::details::shared_state::CancellationContextSharedStateView;
use crate::details::shared_state::CancellationNotificationData;
use crate::details::shared_state::CancellationNotificationFuture;

pub struct ExplicitCriticalSectionGuard<'a> {
    pub(crate) context: Option<&'a CancellationContextSharedStateView>,
    pub(crate) notification: CancellationNotificationData,
}

impl<'a> ExplicitCriticalSectionGuard<'a> {
    pub(crate) fn cancellation_observer(&self) -> CancellationObserver {
        CancellationObserver(CancellationObserverInner::Explicit(
            CancellationNotificationFuture::new(self.notification.dupe()),
        ))
    }

    pub(crate) async fn allow_cancellations_again(self) {
        let context = self.take();
        if context.exit_prevent_cancellation() {
            // TODO(cjhopman): It seems like this is likely unreliable behavior. The intent here is that by returning
            // Poll::Pending at an await point here, that that Poll::Pending be essentially propagated all the way out
            // of the corresponding cancellable future's poll call so that we can reenter that outermost poll and it can notice
            // that we were cancelled.
            //
            // But, just like tokio::task::yield_now(), that isn't guaranteed. If this behavior is unreliable, code using
            // this is already generally really complex and we could be introducing extremely difficult to diagnose bugs
            // by depending on this behavior.

            // If the current future should actually be cancelled now, we try to return
            // control to it immediately to allow cancellation to kick in faster.
            // We don't use tokio's yield now as that changes the scheduling of the future so that
            // the current task gets delayed to the end of the work queue.
            // We simply want to re-enter the poll once to check for cancellation, so put it at the
            // front of the work queue.
            // Additionally, tokio's yield_now seem to fail to wake up tasks in some scenarios
            // https://github.com/tokio-rs/tokio/discussions/5846
            struct YieldAndReenter(bool);

            impl Future for YieldAndReenter {
                type Output = ();

                fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
                    if self.0 {
                        Poll::Ready(())
                    } else {
                        // wake up immediately because we don't care about actually deferring the
                        // future, we just want to re-enter the poll once to check cancellations.
                        cx.waker().wake_by_ref();
                        self.get_mut().0 = true;
                        Poll::Pending
                    }
                }
            }

            YieldAndReenter(false).await
        }
    }

    pub(crate) fn keep_going_on_cancellations_if_not_cancelled(
        self,
    ) -> Option<DisableCancellationGuard> {
        let context = self.take();
        if context.try_to_disable_cancellation() {
            Some(DisableCancellationGuard)
        } else {
            None
        }
    }

    fn take(mut self) -> &'a CancellationContextSharedStateView {
        self.context.take().unwrap()
    }
}

impl<'a> Drop for ExplicitCriticalSectionGuard<'a> {
    fn drop(&mut self) {
        if let Some(context) = &self.context {
            // never actually exited during normal poll, but dropping this means we'll never poll
            // again, so just release the `prevent_cancellation`
            context.exit_prevent_cancellation();
        }
    }
}

pub(crate) struct ExplicitCancellationContext {
    pub(crate) inner: CancellationContextSharedStateView,
}

impl ExplicitCancellationContext {
    /// Ignore cancellations while 'PreventCancellation' is held
    pub fn begin_ignore_cancellation(&self) -> CriticalSectionGuard {
        self.inner.enter_structured_cancellation()
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
        let guard = self.begin_ignore_cancellation();
        async move {
            let r = make().await;

            guard.exit_critical_section().await;

            r
        }
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
        let guard = self.begin_ignore_cancellation();

        async move {
            let r = make(guard.cancellation_observer()).await;

            guard.exit_critical_section().await;

            r
        }
    }
}

pub(crate) enum CancellationContextInner {
    /// A context where the outer future will not be dropped (for example, because it is known to be the outermost
    /// future in a spawn call).
    NeverCancelled,
    /// The cancellation context passed explicitly
    Explicit(ExplicitCancellationContext),
}

impl CancellationContextInner {
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
        match self {
            CancellationContextInner::NeverCancelled => Either::Left(make()),
            CancellationContextInner::Explicit(context) => {
                Either::Right(context.critical_section(make))
            }
        }
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
        match self {
            CancellationContextInner::NeverCancelled => {
                Either::Left(make(CancellationObserver::never_cancelled()))
            }
            CancellationContextInner::Explicit(context) => {
                Either::Right(context.with_structured_cancellation(make))
            }
        }
    }

    pub(crate) fn begin_ignore_cancellation(&self) -> CriticalSectionGuard<'_> {
        match self {
            CancellationContextInner::NeverCancelled => CriticalSectionGuard::NeverCancelled,
            CancellationContextInner::Explicit(inner) => inner.begin_ignore_cancellation(),
        }
    }
}
