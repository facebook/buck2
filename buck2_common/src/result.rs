/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::sync::Arc;

use gazebo::prelude::*;

/// SharedError is a simple, cloneable Error wrapper. It holds the inner error in an Arc to support Clone.
///
/// Propagation of errors via `?` convert automatically between error types, but propagating results directly
/// as different error types requires use of `shared_error()` or `unshared_error()`.
///
/// ```
/// use buck2_common::result::*;
///
/// fn foo() -> Result<(), anyhow::Error> {
///     bar()?;
///     bar().unshared_error()
/// }
///
/// fn bar() -> Result<(), SharedError> {
///     foo()?;
///     io()?;
///     foo().shared_error()
/// }
///
/// fn io() -> Result<(), std::io::Error> {
///     Ok(())
/// }
///
/// ```
///
/// Caveat: Each std::error::Error implementation requires its own explicit `impl From<E> for SharedError`, so we add those as needed.
#[derive(Debug, Clone, Dupe)]
pub struct SharedError(Arc<anyhow::Error>);

impl SharedError {
    pub fn new(e: impl Into<anyhow::Error>) -> SharedError {
        SharedError(Arc::new(e.into()))
    }

    pub fn inner(&self) -> &anyhow::Error {
        &self.0
    }
}

impl Display for SharedError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // Normally when displaying an anyhow, we format with {:#}.
        // However, because we also give up source(), that can lead
        // to O(n^2) output. See the test at the bottom of this file.
        Display::fmt(&self.0, f)
    }
}

impl std::error::Error for SharedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }

    fn backtrace(&self) -> Option<&std::backtrace::Backtrace> {
        Some(self.0.backtrace())
    }

    fn cause(&self) -> Option<&dyn std::error::Error> {
        #[allow(deprecated)]
        self.0.cause()
    }
}

impl From<&SharedError> for SharedError {
    fn from(v: &SharedError) -> Self {
        v.dupe()
    }
}

impl From<anyhow::Error> for SharedError {
    fn from(err: anyhow::Error) -> Self {
        SharedError(Arc::new(err))
    }
}

impl From<std::io::Error> for SharedError {
    fn from(err: std::io::Error) -> Self {
        SharedError(Arc::new(err.into()))
    }
}

impl From<tokio::task::JoinError> for SharedError {
    fn from(err: tokio::task::JoinError) -> Self {
        SharedError(Arc::new(err.into()))
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
    fn unshared_error(self) -> Result<T, E>;
}

impl<T> ToUnsharedResultExt<T, anyhow::Error> for SharedResult<T> {
    fn unshared_error(self) -> Result<T, anyhow::Error> {
        self.map_err(|e| e.into())
    }
}

/// Like `downcast_ref()`, but if the error is a [`SharedError`], attempt to downcast
/// its `inner()` error instead.
pub fn shared_downcast_ref<'a, E: std::error::Error + Display + Debug + Send + Sync + 'static>(
    error: &'a (dyn std::error::Error + 'static),
) -> Option<&'a E> {
    match error.downcast_ref::<E>() {
        Some(e) => Some(e),
        None => match error.downcast_ref::<SharedError>() {
            Some(shared_err) => shared_err.inner().downcast_ref::<E>(),
            None => None,
        },
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use super::*;

    // Which ~VALUE words are in the string
    // Use {||} to make sure we only get real signal
    fn parts(x: &str) -> Vec<&str> {
        let mut res: Vec<&str> = x
            .split_whitespace()
            .filter_map(|x| x.strip_prefix('~'))
            .collect();
        res.sort_unstable();
        res
    }

    #[test]
    fn test_shared_error_display() {
        // With anyhow/context its easy to either lose the original error (if you forget {:#}),
        // and also easy to get O(n^2) instances if you do alternative formatting, AND
        // anyhow looks at Error::source (which it does with alternative formatting).

        let x = anyhow!(" ~A ");
        assert_eq!(parts(&format!("{}", x)), &["A"]);
        assert_eq!(parts(&format!("{:#}", x)), &["A"]);

        let x = x.context(" ~B ");
        assert_eq!(parts(&format!("{}", x)), &["B"]);
        assert_eq!(parts(&format!("{:#}", x)), &["A", "B"]);

        let x: SharedError = x.into();
        assert_eq!(parts(&format!("{}", x)), &["B"]);
        assert_eq!(parts(&format!("{:#}", x)), &["A", "B"]);

        let x: anyhow::Error = x.into();
        assert_eq!(parts(&format!("{}", x)), &["B"]);
        assert_eq!(parts(&format!("{:#}", x)), &["A", "B"]);

        let x = x.context(" ~C ");
        assert_eq!(parts(&format!("{}", x)), &["C"]);
        assert_eq!(parts(&format!("{:#}", x)), &["A", "B", "C"]);
    }

    #[derive(thiserror::Error, Debug)]
    #[error("Test error")]
    struct TestError {
        source: Option<anyhow::Error>,
    }

    #[derive(thiserror::Error, Debug)]
    #[error("Inner error")]
    struct TestInnerError;

    #[test]
    fn test_anyhow_chain_works() {
        let err: anyhow::Error = TestError { source: None }.into();
        let err_with_source: anyhow::Error = TestError {
            source: Some(TestInnerError {}.into()),
        }
        .into();

        let shared_err: anyhow::Error = SharedError::from(err).into();
        let shared_err_with_source: anyhow::Error = SharedError::from(err_with_source).into();

        let found_err = shared_err
            .chain()
            .find_map(|e| shared_downcast_ref::<TestError>(e));
        assert!(found_err.is_some());

        let found_err = shared_err_with_source
            .chain()
            .find_map(|e| shared_downcast_ref::<TestInnerError>(e));
        assert!(found_err.is_some());
    }
}
