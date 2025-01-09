/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

impl From<std::num::ParseIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseIntError) -> Self {
        crate::conversion::from_any(value)
    }
}

impl From<std::num::ParseFloatError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseFloatError) -> Self {
        crate::conversion::from_any(value)
    }
}

impl From<std::num::TryFromIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::TryFromIntError) -> Self {
        crate::conversion::from_any(value)
    }
}
