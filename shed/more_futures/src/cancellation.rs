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
use std::sync::Arc;

use crate::cancellable_future::critical_section;

/// Context available to the function running inside the future to control and manage it's own
/// cancellation
pub struct CancellationContext(Arc<CancellationContextShared>);

impl CancellationContext {
    /// TODO replace with real initialization
    pub fn todo() -> Self {
        Self(Arc::new(CancellationContextShared {}))
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
        critical_section(make)
    }
}

#[allow(unused)]
struct CancellationContextShared {}
