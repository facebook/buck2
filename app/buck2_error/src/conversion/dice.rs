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
use dice_error::result::CancellationReason;

use crate::ErrorTag;

impl From<dice_error::DiceError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: dice_error::DiceError) -> Self {
        let (error_tag, reason) = match *value.0 {
            DiceErrorImpl::DuplicateChange(_) => (ErrorTag::DiceDuplicatedChange, None),
            DiceErrorImpl::ChangedToInvalid(_) => (ErrorTag::DiceChangedToInvalid, None),
            DiceErrorImpl::InjectedKeyGotInvalidation(_) => {
                (ErrorTag::DiceInjectedKeyGotInvalidation, None)
            }
            DiceErrorImpl::Cancelled(CancellationReason::Rejected) => {
                (ErrorTag::DiceRejected, None)
            }
            DiceErrorImpl::Cancelled(reason) => (ErrorTag::DiceCancelled, Some(reason)),
            DiceErrorImpl::UnexpectedCycleGuardType { .. } => {
                (ErrorTag::DiceUnexpectedCycleGuardType, None)
            }
            DiceErrorImpl::DuplicateActivationData => (ErrorTag::DiceDuplicateActivationData, None),
        };

        let mut error = crate::conversion::from_any_with_tag(value, error_tag);
        if let Some(reason) = reason {
            error = error.string_tag(&reason.to_string());
        }
        error
    }
}

impl From<DetectCyclesParseError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: DetectCyclesParseError) -> Self {
        crate::conversion::from_any_with_tag(value, ErrorTag::Input)
    }
}
