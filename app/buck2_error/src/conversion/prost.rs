/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

impl From<prost::EncodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::EncodeError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Prost)
    }
}

impl From<prost::DecodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::DecodeError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Prost)
    }
}

impl From<prost::UnknownEnumValue> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::UnknownEnumValue) -> Self {
        // Replacement for 'DecodeError' in some circumstances
        // https://github.com/tokio-rs/prost/blob/master/CHANGELOG.md#prost-version-0130
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Prost)
    }
}

impl From<prost_types::DurationError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost_types::DurationError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Prost)
    }
}

impl From<prost_types::TimestampError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost_types::TimestampError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Prost)
    }
}
