/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::ErrorTag::Http;
use crate::conversion::from_any_with_tag;

impl From<http::uri::InvalidUri> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::uri::InvalidUri) -> Self {
        from_any_with_tag(value, Http)
    }
}

impl From<http::uri::InvalidUriParts> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::uri::InvalidUriParts) -> Self {
        from_any_with_tag(value, Http)
    }
}

impl From<http::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::Error) -> Self {
        from_any_with_tag(value, Http)
    }
}
