/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::conversion::from_any_with_tag;

impl From<std::string::FromUtf8Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::string::FromUtf8Error) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StringUtf8)
    }
}

// TODO get rid of this
impl From<std::string::String> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::string::String) -> Self {
        let source_location =
            crate::source_location::SourceLocation::new(std::panic::Location::caller().file());

        crate::Error::new(
            value,
            crate::ErrorTag::StringConversion,
            source_location,
            None,
        )
    }
}

impl From<std::str::Utf8Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::str::Utf8Error) -> Self {
        from_any_with_tag(value, crate::ErrorTag::StringUtf8)
    }
}

impl From<std::str::ParseBoolError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::str::ParseBoolError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::ParseBool)
    }
}
