/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::{self};

use thiserror::Error;

#[derive(Error, Debug)]
#[error("{}", .message)]
pub struct REClientError {
    pub message: String,
    pub code: TCode,
}

#[derive(Debug)]
pub struct REError {
    pub code: TCode,
    pub message: String,
    pub error_location: ErrorLocation,
}

#[derive(Debug)]
pub struct ErrorLocation(pub ::std::primitive::i32);

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNKNOWN")
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct TCode(pub i32);

impl TCode {
    pub const OK: Self = TCode(0i32);
    pub const INVALID_ARGUMENT: Self = TCode(3i32);
    pub const NOT_FOUND: Self = TCode(5i32);
}

impl Display for TCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self == &TCode::OK {
            write!(f, "OK")
        } else if self == &TCode::INVALID_ARGUMENT {
            write!(f, "INVALID_ARGUMENT")
        } else {
            write!(f, "UNKNOWN")
        }
    }
}
