/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub type SharedError = crate::Error;

pub type SharedResult<T> = Result<T, SharedError>;

/// shared_error() can convert some non-shared Result to Result.
///
/// The `?` operator will automatically convert non-shared Error to SharedError, so shared_error()
/// is usually unnecessary except when propagating a Result directly as a return value.
pub trait ToSharedResultExt<T> {
    fn shared_error(self) -> SharedResult<T>;
}

impl<T, E> ToSharedResultExt<T> for Result<T, E>
where
    SharedError: From<E>,
{
    fn shared_error(self) -> SharedResult<T> {
        Ok(self?)
    }
}

pub trait ToUnsharedResultExt<T> = crate::Context<T>;
