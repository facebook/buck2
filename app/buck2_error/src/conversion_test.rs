/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Test conversion impls for different error types to 'buck2_error::Error'

use crate::conversion::from_any_with_tag;

#[derive(Debug, thiserror::Error)]
#[error("My error")]
pub(crate) struct MyError;

impl From<MyError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: MyError) -> Self {
        from_any_with_tag(value, crate::ErrorTag::Input)
    }
}
