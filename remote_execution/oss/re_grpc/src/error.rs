/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use dupe::Dupe;
use thiserror::Error;

#[derive(Error, Debug, Clone, Default)]
#[error("{}", .message)]
pub struct REClientError {
    pub message: String,
    pub code: TCode,
    pub group: TCodeReasonGroup,
}

#[derive(Debug, Clone, Dupe, Default)]
pub struct ErrorLocation(pub ::std::primitive::i32);

impl Display for ErrorLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNKNOWN")
    }
}

#[derive(Copy, Debug, PartialEq, Eq, Clone, Dupe, Default)]
pub struct TCode(pub i32);

impl TCode {
    pub const OK: Self = TCode(0i32);
    pub const CANCELLED: Self = TCode(1i32);
    pub const UNKNOWN: Self = TCode(2i32);
    pub const INVALID_ARGUMENT: Self = TCode(3i32);
    pub const DEADLINE_EXCEEDED: Self = TCode(4i32);
    pub const NOT_FOUND: Self = TCode(5i32);
    pub const ALREADY_EXISTS: Self = TCode(6i32);
    pub const PERMISSION_DENIED: Self = TCode(7i32);
    pub const RESOURCE_EXHAUSTED: Self = TCode(8i32);
    pub const FAILED_PRECONDITION: Self = TCode(9i32);
    pub const ABORTED: Self = TCode(10i32);
    pub const OUT_OF_RANGE: Self = TCode(11i32);
    pub const UNIMPLEMENTED: Self = TCode(12i32);
    pub const INTERNAL: Self = TCode(13i32);
    pub const UNAVAILABLE: Self = TCode(14i32);
    pub const DATA_LOSS: Self = TCode(15i32);
    pub const UNAUTHENTICATED: Self = TCode(16i32);
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

#[derive(Copy, Debug, PartialEq, Eq, Clone, Dupe, Default)]
pub struct TCodeReasonGroup(pub i32);

impl TCodeReasonGroup {
    pub const UNKNOWN: Self = TCodeReasonGroup(0i32);
}

impl Display for TCodeReasonGroup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UNKNOWN")
    }
}
