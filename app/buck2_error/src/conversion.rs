/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Conversion impls for different error types to 'buck2_error::Error'

use std::string::FromUtf8Error;
use std::sync::Arc;

use dice_error::DiceError;
use relative_path::FromPathError;

use crate::any::recover_crate_error;
use crate::any::CrateAsStdError;

// Helper function that can be explicited called to convert `std::error::Error` into `buck2_error`.
// Common types should have a proper From implemented in this file, but this function is useful for
// one-off error types in the codebase
#[cold]
#[track_caller]
pub fn from_any<T>(e: T) -> crate::Error
where
    T: Into<anyhow::Error>,
    // This bound prevent `from_any` from being called on an error that's
    // already a `buck2_error` which prevents unecessary uses of `from_any`
    Result<(), T>: anyhow::Context<(), T>,
{
    let anyhow: anyhow::Error = e.into();
    let source_location =
        crate::source_location::from_file(std::panic::Location::caller().file(), None);
    recover_crate_error(anyhow.as_ref(), source_location)
}

impl From<crate::Error> for anyhow::Error {
    #[cold]
    #[track_caller]
    fn from(value: crate::Error) -> Self {
        Into::into(CrateAsStdError(value))
    }
}

impl From<FromPathError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: FromPathError) -> Self {
        from_any(value)
    }
}

impl From<FromUtf8Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: FromUtf8Error) -> Self {
        from_any(value)
    }
}

impl From<std::str::Utf8Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::str::Utf8Error) -> Self {
        from_any(value)
    }
}

impl From<DiceError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: DiceError) -> Self {
        from_any(value)
    }
}

impl<T: Sync + Send + 'static> From<tokio::sync::mpsc::error::SendError<T>> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::mpsc::error::SendError<T>) -> Self {
        from_any(value)
    }
}

impl From<tokio::task::JoinError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::task::JoinError) -> Self {
        from_any(value)
    }
}

impl From<tokio::sync::AcquireError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::AcquireError) -> Self {
        from_any(value)
    }
}

impl From<tokio::time::error::Elapsed> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::time::error::Elapsed) -> Self {
        from_any(value)
    }
}

impl From<tokio::sync::oneshot::error::RecvError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::oneshot::error::RecvError) -> Self {
        from_any(value)
    }
}

impl From<rusqlite::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: rusqlite::Error) -> Self {
        from_any(value)
    }
}

impl From<regex::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: regex::Error) -> Self {
        from_any(value)
    }
}

impl From<fancy_regex::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: fancy_regex::Error) -> Self {
        from_any(value)
    }
}

#[cfg(unix)]
impl From<nix::errno::Errno> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: nix::errno::Errno) -> Self {
        from_any(value)
    }
}

impl From<tonic::transport::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tonic::transport::Error) -> Self {
        from_any(value)
    }
}

impl From<tonic::Status> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tonic::Status) -> Self {
        from_any(value)
    }
}

impl From<prost::EncodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::EncodeError) -> Self {
        from_any(value)
    }
}

impl From<prost::DecodeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost::DecodeError) -> Self {
        from_any(value)
    }
}

impl From<prost_types::DurationError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost_types::DurationError) -> Self {
        from_any(value)
    }
}

impl From<prost_types::TimestampError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: prost_types::TimestampError) -> Self {
        from_any(value)
    }
}

impl From<std::string::String> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::string::String) -> Self {
        let source_location =
            crate::source_location::from_file(std::panic::Location::caller().file(), None);

        crate::Error::new(value, source_location, None)
    }
}

impl From<std::num::TryFromIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::TryFromIntError) -> Self {
        from_any(value)
    }
}

impl From<std::time::SystemTimeError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::time::SystemTimeError) -> Self {
        from_any(value)
    }
}

impl From<std::array::TryFromSliceError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::array::TryFromSliceError) -> Self {
        from_any(value)
    }
}

impl From<std::path::StripPrefixError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::path::StripPrefixError) -> Self {
        from_any(value)
    }
}

// Conversions below are needed for source/transparent conversions
impl From<std::io::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::io::Error) -> Self {
        from_any(value)
    }
}

impl From<serde_json::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: serde_json::Error) -> Self {
        from_any(value)
    }
}

impl From<http::uri::InvalidUri> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::uri::InvalidUri) -> Self {
        from_any(value)
    }
}

impl From<http::uri::InvalidUriParts> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::uri::InvalidUriParts) -> Self {
        from_any(value)
    }
}

impl From<http::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: http::Error) -> Self {
        from_any(value)
    }
}

impl From<hyper::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: hyper::Error) -> Self {
        from_any(value)
    }
}

impl From<Arc<crate::Error>> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: Arc<crate::Error>) -> Self {
        (*value).clone()
    }
}

impl From<hex::FromHexError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: hex::FromHexError) -> Self {
        from_any(value)
    }
}

#[cfg(fbcode_build)]
impl From<edenfs_clients::errors::ListMountsError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: edenfs_clients::errors::ListMountsError) -> Self {
        from_any(value)
    }
}

impl From<watchman_client::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: watchman_client::Error) -> Self {
        from_any(value)
    }
}

// Conversions below are needed for buck2_common/src/init.rs parse functions
impl From<std::num::ParseIntError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseIntError) -> Self {
        from_any(value)
    }
}

impl From<std::convert::Infallible> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::convert::Infallible) -> Self {
        from_any(value)
    }
}

impl From<std::str::ParseBoolError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::str::ParseBoolError) -> Self {
        from_any(value)
    }
}

impl From<std::num::ParseFloatError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::num::ParseFloatError) -> Self {
        from_any(value)
    }
}
