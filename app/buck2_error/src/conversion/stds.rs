/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub(crate) mod io;
mod string;

use crate::conversion::from_any_with_tag;

impl From<std::array::TryFromSliceError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::array::TryFromSliceError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StdSlice)
    }
}

impl From<std::time::SystemTimeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::time::SystemTimeError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StdTime)
    }
}

impl From<std::convert::Infallible> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::convert::Infallible) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StdInfallible)
    }
}

impl From<std::path::StripPrefixError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::path::StripPrefixError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StdStripPrefix)
    }
}

impl From<std::num::ParseIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseIntError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::ParseNum)
    }
}

impl From<std::num::ParseFloatError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseFloatError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::ParseNum)
    }
}

impl From<std::num::TryFromIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::TryFromIntError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::IntConversion)
    }
}

impl From<std::ffi::NulError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::ffi::NulError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::CstringNul)
    }
}
