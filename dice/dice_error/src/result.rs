/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Cancellable results of DICE

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use thiserror::Error;

pub type CancellableResult<T> = Result<T, CancellationReason>;

#[derive(Clone, Dupe, Copy, Display, Debug, Error, Allocative, PartialEq)]
#[display("{:?}", self)]
pub enum CancellationReason {
    OutdatedEpoch,
    Rejected,
    DepsMatch,
    WorkerFinished,
    Cached,
    AllDependentsDropped,
    TransactionCancelled,
    TransactionDropped,
    /// Used by test code that manually cancels things.
    ByTest,
    /// Indicates the DiceTaskHandle was dropped without producing any result or (other) cancellation.
    HandleDropped,
    /// Hydrating a paged-out value via `DiceStorage` failed (e.g. backing-store I/O error
    /// or deserialization failure). Treated like `Rejected` — the worker terminates cleanly.
    HydrationFailure,
}
