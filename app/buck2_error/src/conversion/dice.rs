/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use dice_error::DiceErrorImpl;
use dice_error::cycles::DetectCyclesParseError;
use dice_error::storage::PagableStorageBackendParseError;

use crate::ErrorTag;

impl From<dice_error::DiceError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: dice_error::DiceError) -> Self {
        let error_tag = match *value.0 {
            DiceErrorImpl::DuplicateChange(_) => ErrorTag::DiceDuplicatedChange,
            DiceErrorImpl::ChangedToInvalid(_) => ErrorTag::DiceChangedToInvalid,
            DiceErrorImpl::InjectedKeyGotInvalidation(_) => {
                ErrorTag::DiceInjectedKeyGotInvalidation
            }
            // `DiceRejected` is a weird name for this, but is what it was historically, so keeping
            // it
            DiceErrorImpl::TransactionCancelled => ErrorTag::DiceRejected,
            DiceErrorImpl::UnexpectedCycleGuardType { .. } => {
                ErrorTag::DiceUnexpectedCycleGuardType
            }
            DiceErrorImpl::DuplicateActivationData => ErrorTag::DiceDuplicateActivationData,
        };

        crate::conversion::from_any_with_tag(value, error_tag)
    }
}

impl From<DetectCyclesParseError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: DetectCyclesParseError) -> Self {
        crate::conversion::from_any_with_tag(value, ErrorTag::Input)
    }
}

impl From<PagableStorageBackendParseError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: PagableStorageBackendParseError) -> Self {
        crate::conversion::from_any_with_tag(value, ErrorTag::Input)
    }
}
