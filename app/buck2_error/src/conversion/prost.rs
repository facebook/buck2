/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

impl From<prost::EncodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::EncodeError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Tier0)
    }
}

impl From<prost::DecodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::DecodeError) -> Self {
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Tier0)
    }
}
