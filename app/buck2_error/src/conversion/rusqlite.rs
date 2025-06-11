/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use derive_more::Display;

impl From<rusqlite::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: rusqlite::Error) -> Self {
        if let rusqlite::Error::FromSqlConversionFailure(_, _, e) = &value {
            if let Some(wrapper) = e.downcast_ref::<Buck2ErrorAsRusqliteError>() {
                return wrapper.0.clone();
            }
        }
        crate::conversion::from_any_with_tag(value, crate::ErrorTag::Rusqlite)
    }
}

#[derive(Debug, Display)]
#[repr(transparent)]
pub struct Buck2ErrorAsRusqliteError(pub crate::Error);

impl std::error::Error for Buck2ErrorAsRusqliteError {}
