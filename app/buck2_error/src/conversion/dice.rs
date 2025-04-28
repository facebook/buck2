/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dice_error::DiceErrorImpl;

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
            // TODO(minglunli): Might be worth extracting the CancellationReason and create tags for those too
            DiceErrorImpl::Cancelled(_) => ErrorTag::DiceCancelled,
            DiceErrorImpl::UnexpectedCycleGuardType {
                expected_type_name: _,
                actual_type_name: _,
            } => ErrorTag::DiceUnexpectedCycleGuardType,
            DiceErrorImpl::DuplicateActivationData => ErrorTag::DiceDuplicateActivationData,
        };

        crate::conversion::from_any_with_tag(value, error_tag)
    }
}
