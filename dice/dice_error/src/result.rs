/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Cancellable results of DICE

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use thiserror::Error;

pub type CancellableResult<T> = Result<T, CancellationReason>;

#[derive(Clone, Dupe, Copy, Display, Debug, Error, Allocative)]
#[display("{:?}", self)]
pub enum CancellationReason {
    OutdatedEpoch,
    Rejected,
    DepsMatch,
    WorkerFinished,
    Cached,
    AllDependentsDropped,
    AllObserversDropped,
    TransactionCancelled,
    TransactionDropped,
    /// Used by test code that manually cancels things.
    ByTest,
    /// Indicates the DiceTaskHandle was dropped without producing any result or (other) cancellation.
    HandleDropped,
}
