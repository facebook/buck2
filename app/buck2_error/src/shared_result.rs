/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use dupe::Dupe;

pub type SharedError = crate::Error;

impl From<&SharedError> for anyhow::Error {
    fn from(e: &SharedError) -> Self {
        e.dupe().into()
    }
}

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

/// unshared_error() can be used to convert a SharedResult to a normal anyhow::Result. The inner error will still be shared.
///
/// The `?` operator will automatically convert SharedError to non-shared Error in the same way,
/// so unshared_error() is only necessary when propagating a SharedResult directly.
pub trait ToUnsharedResultExt<T, E> {
    fn with_context<C, F>(self, f: F) -> anyhow::Result<T>
    where
        C: fmt::Display + Send + Sync + 'static,
        F: FnOnce() -> C;

    fn unshared_error(self) -> Result<T, E>;
}

impl<T> ToUnsharedResultExt<T, anyhow::Error> for SharedResult<T> {
    fn with_context<C, F>(self, f: F) -> anyhow::Result<T>
    where
        C: fmt::Display + Send + Sync + 'static,
        F: FnOnce() -> C,
    {
        crate::Context::with_context(self, f)
    }

    fn unshared_error(self) -> Result<T, anyhow::Error> {
        self.map_err(|e| e.into())
    }
}
