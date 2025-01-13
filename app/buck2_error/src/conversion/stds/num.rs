/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::conversion::from_any_with_tag;
use crate::ErrorTag::Tier0;

impl From<std::num::ParseIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseIntError) -> Self {
        from_any_with_tag(value, Tier0)
    }
}

impl From<std::num::ParseFloatError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseFloatError) -> Self {
        from_any_with_tag(value, Tier0)
    }
}

impl From<std::num::TryFromIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::TryFromIntError) -> Self {
        from_any_with_tag(value, Tier0)
    }
}
