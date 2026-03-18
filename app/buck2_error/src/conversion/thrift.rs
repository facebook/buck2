/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use fbthrift::ApplicationExceptionErrorCode;
use fbthrift::errors::NonthrowingFunctionError;

use crate::ErrorTag;

impl From<NonthrowingFunctionError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: NonthrowingFunctionError) -> Self {
        let extra_tag = match &value {
            NonthrowingFunctionError::ApplicationException(e) => match e.type_ {
                ApplicationExceptionErrorCode::Timeout => Some(ErrorTag::ThriftTimeout),
                ApplicationExceptionErrorCode::Loadshedding => Some(ErrorTag::ThriftLoadshedding),
                _ => None,
            },
            NonthrowingFunctionError::ThriftError(_) => None,
        };
        let mut err = crate::conversion::from_any_with_tag(value, ErrorTag::Thrift);
        if let Some(tag) = extra_tag {
            err = err.tag([tag]);
        }
        err
    }
}
