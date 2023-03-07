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
use indexmap::IndexSet;
use itertools::Itertools;
use thiserror::Error;

use crate::legacy::cycles::RequestedKey;

#[derive(Clone, Dupe, Debug, Error, Allocative)]
#[error(transparent)]
pub struct DiceError(pub(crate) Arc<DiceErrorImpl>);

impl DiceError {
    pub fn cycle(
        trigger: Arc<dyn RequestedKey>,
        cyclic_keys: IndexSet<Arc<dyn RequestedKey>>,
    ) -> Self {
        DiceError(Arc::new(DiceErrorImpl::Cycle {
            trigger,
            cyclic_keys,
        }))
    }

    pub fn duplicate(key: Arc<dyn RequestedKey>) -> Self {
        DiceError(Arc::new(DiceErrorImpl::DuplicateChange(key)))
    }

    pub fn cancelled() -> Self {
        DiceError(Arc::new(DiceErrorImpl::Cancelled))
    }
}

#[derive(Debug, Error, Allocative)]
pub(crate) enum DiceErrorImpl {
    #[error("Cyclic computation detected when computing key `{}`, which forms a cycle in computation chain: `{}`", trigger, cyclic_keys.iter().join(","))]
    Cycle {
        trigger: Arc<dyn RequestedKey>,
        cyclic_keys: IndexSet<Arc<dyn RequestedKey>>,
    },
    #[error("Key `{0}` was marked as changed multiple times on the same transaction.")]
    DuplicateChange(Arc<dyn RequestedKey>),
    /// NOTE: This isn't an error users normally see, since if the user is waiting on a result, the
    /// future doesn't get cancelled.
    #[error("The evaluation of this key was cancelled")]
    Cancelled,
    #[error(
        "Requested cycle_guard of type {}, but current guard has type {}",
        expected_type_name,
        actual_type_name
    )]
    UnexpectedCycleGuardType {
        expected_type_name: String,
        actual_type_name: String,
    },
}

pub type DiceResult<T> = Result<T, DiceError>;
