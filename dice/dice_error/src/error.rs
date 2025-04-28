/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use thiserror::Error;

use crate::cycles::RequestedKey;
use crate::result::CancellationReason;

#[derive(Clone, Dupe, Debug, Error, Allocative)]
#[error(transparent)]
pub struct DiceError(pub Arc<DiceErrorImpl>);

impl DiceError {
    pub fn invalid_change(key: Arc<dyn RequestedKey>) -> Self {
        DiceError(Arc::new(DiceErrorImpl::ChangedToInvalid(key)))
    }

    pub fn duplicate(key: Arc<dyn RequestedKey>) -> Self {
        DiceError(Arc::new(DiceErrorImpl::DuplicateChange(key)))
    }

    pub fn cancelled(reason: CancellationReason) -> Self {
        DiceError(Arc::new(DiceErrorImpl::Cancelled(reason)))
    }

    pub fn duplicate_activation_data() -> Self {
        DiceError(Arc::new(DiceErrorImpl::DuplicateActivationData))
    }

    pub fn injected_key_invalidated(key: Arc<dyn RequestedKey>) -> Self {
        DiceError(Arc::new(DiceErrorImpl::InjectedKeyGotInvalidation(key)))
    }
}

#[derive(Debug, Error, Allocative)]
pub enum DiceErrorImpl {
    #[error("Key `{0}` was marked as changed multiple times on the same transaction.")]
    DuplicateChange(Arc<dyn RequestedKey>),
    #[error("Key `{0}` was reported as changed to an invalid value")]
    ChangedToInvalid(Arc<dyn RequestedKey>),
    #[error("Key `{0}` is an InjectedKey and received an invalidation")]
    InjectedKeyGotInvalidation(Arc<dyn RequestedKey>),
    /// NOTE: This isn't an error users normally see, since if the user is waiting on a result, the
    /// future doesn't get cancelled.
    #[error("The evaluation of this key was cancelled: {0}")]
    Cancelled(CancellationReason),
    #[error(
        "Requested cycle_guard of type {}, but current guard has type {}",
        expected_type_name,
        actual_type_name
    )]
    UnexpectedCycleGuardType {
        expected_type_name: String,
        actual_type_name: String,
    },
    #[error("Activation data was already provided for this key")]
    DuplicateActivationData,
}

pub type DiceResult<T> = Result<T, DiceError>;
