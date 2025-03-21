/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use smallvec::smallvec;

use crate::context_value::ContextValue;
use crate::context_value::TypedContext;
use crate::{self as buck2_error};

/// Provides the `context` method for `Result`.
///
/// This trait is analogous to the `anyhow::Context` trait. It is mostly a drop-in replacement, and
/// in the near future, uses of `anyhow::Context` in `buck2/app` will be broadly replaced with use
/// of this trait. Subsequently, additional APIs will be provided for annotating errors with
/// structured context data.
pub trait BuckErrorContext<T>: Sealed {
    #[track_caller]
    fn buck_error_context<C: Into<ContextValue>>(self, context: C) -> crate::Result<T>;

    #[track_caller]
    fn with_buck_error_context<C, F>(self, f: F) -> crate::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C;

    #[track_caller]
    fn tag(self, tag: crate::ErrorTag) -> crate::Result<T> {
        self.buck_error_context(ContextValue::Tags(smallvec![tag]))
    }

    #[track_caller]
    fn internal_error(self, message: &str) -> crate::Result<T> {
        self.with_internal_error(|| message.to_owned())
    }

    #[track_caller]
    fn with_internal_error<F>(self, f: F) -> crate::Result<T>
    where
        F: FnOnce() -> String,
    {
        self.with_buck_error_context(|| format!("{} (internal error)", f()))
            .tag(crate::ErrorTag::InternalError)
    }

    /// Code below returns an anyhow::Error, it is used while we transition from anyhow to buck2_error in buck2/app
    /// TODO(minglunli): Delete the code below once we have fully transitioned to buck2_error
    #[track_caller]
    fn buck_error_context_anyhow<C: Into<ContextValue>>(self, context: C) -> anyhow::Result<T>;

    #[track_caller]
    fn with_buck_error_context_anyhow<C, F>(self, f: F) -> anyhow::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C;

    /// Supports adding context to an error by either augmenting the most recent context if its
    /// the requested type or by adding a new context.
    #[track_caller]
    fn compute_context<
        TC: TypedContext,
        C1: Into<ContextValue>,
        C2: Into<ContextValue>,
        F: FnOnce(Arc<TC>) -> C1,
        F2: FnOnce() -> C2,
    >(
        self,
        map_context: F,
        new_context: F2,
    ) -> crate::Result<T>;
}
pub trait Sealed: Sized {}

impl<T, E> Sealed for std::result::Result<T, E> where crate::Error: From<E> {}

impl<T, E> BuckErrorContext<T> for std::result::Result<T, E>
where
    crate::Error: From<E>,
{
    fn buck_error_context<C>(self, c: C) -> crate::Result<T>
    where
        C: Into<ContextValue>,
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(crate::Error::from(e).context(c)),
        }
    }

    fn with_buck_error_context<C, F>(self, f: F) -> crate::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C,
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(crate::Error::from(e).context(f())),
        }
    }

    fn buck_error_context_anyhow<C>(self, c: C) -> anyhow::Result<T>
    where
        C: Into<ContextValue>,
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(crate::Error::new_anyhow_with_context(e, c)),
        }
    }

    fn with_buck_error_context_anyhow<C, F>(self, f: F) -> anyhow::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C,
    {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(crate::Error::new_anyhow_with_context(e, f())),
        }
    }

    #[track_caller]
    fn compute_context<
        TC: TypedContext,
        C1: Into<ContextValue>,
        C2: Into<ContextValue>,
        F: FnOnce(Arc<TC>) -> C1,
        F2: FnOnce() -> C2,
    >(
        self,
        map_context: F,
        new_context: F2,
    ) -> crate::Result<T> {
        match self {
            Ok(x) => Ok(x),
            Err(e) => Err(crate::Error::from(e).compute_context(map_context, new_context)),
        }
    }
}

#[derive(Debug, buck2_error_derive::Error)]
#[error("NoneError")]
#[buck2(tag = UnexpectedNone)]
struct NoneError;

impl<T> Sealed for Option<T> {}

impl<T> BuckErrorContext<T> for Option<T> {
    fn buck_error_context<C>(self, c: C) -> crate::Result<T>
    where
        C: Into<ContextValue>,
    {
        match self {
            Some(x) => Ok(x),
            None => Err(crate::Error::from(NoneError).context(c)),
        }
    }

    fn with_buck_error_context<C, F>(self, f: F) -> crate::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C,
    {
        match self {
            Some(x) => Ok(x),
            None => Err(crate::Error::from(NoneError).context(f())),
        }
    }

    fn buck_error_context_anyhow<C>(self, c: C) -> anyhow::Result<T>
    where
        C: Into<ContextValue>,
    {
        match self {
            Some(x) => Ok(x),
            None => Err(crate::Error::new_anyhow_with_context(NoneError, c)),
        }
    }

    fn with_buck_error_context_anyhow<C, F>(self, f: F) -> anyhow::Result<T>
    where
        C: Into<ContextValue>,
        F: FnOnce() -> C,
    {
        match self {
            Some(x) => Ok(x),
            None => Err(crate::Error::new_anyhow_with_context(NoneError, f())),
        }
    }

    #[track_caller]
    fn compute_context<
        TC: TypedContext,
        C1: Into<ContextValue>,
        C2: Into<ContextValue>,
        F: FnOnce(Arc<TC>) -> C1,
        F2: FnOnce() -> C2,
    >(
        self,
        _map_context: F,
        new_context: F2,
    ) -> crate::Result<T> {
        Err(crate::Error::from(NoneError).context(new_context()))
    }
}

#[cfg(test)]
mod tests {
    use std::any::Any;
    use std::error::Error as StdError;
    use std::fmt::Display;

    use allocative::Allocative;

    use super::*;
    use crate::source_location::SourceLocation;

    #[derive(Debug, derive_more::Display)]
    struct TestError;

    impl StdError for TestError {}

    impl From<TestError> for crate::Error {
        #[cold]
        fn from(_: TestError) -> Self {
            crate::Error::new(
                "".to_owned(),
                crate::ErrorTag::Input,
                SourceLocation::new(file!()),
                None,
            )
        }
    }

    #[derive(Debug, Allocative, Eq, PartialEq)]
    struct SomeContext(Vec<u32>);

    impl Display for SomeContext {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?}", self)
        }
    }

    impl TypedContext for SomeContext {
        fn eq(&self, other: &dyn TypedContext) -> bool {
            match (other as &dyn Any).downcast_ref::<Self>() {
                Some(v) => self == v,
                None => false,
            }
        }
    }

    #[test]
    fn test_compute_context() {
        crate::Error::check_equal(
            &crate::Error::from(TestError).context("string"),
            &crate::Error::from(TestError).compute_context(
                |_t: Arc<SomeContext>| -> SomeContext { panic!() },
                || "string",
            ),
        );

        crate::Error::check_equal(
            &crate::Error::from(TestError).context(SomeContext(vec![0, 1, 2])),
            &crate::Error::from(TestError)
                .context(SomeContext(vec![]))
                .compute_context(
                    |_t: Arc<SomeContext>| -> SomeContext { SomeContext(vec![0, 1, 2]) },
                    || "string",
                ),
        );

        crate::Error::check_equal(
            &crate::Error::from(Option::<()>::None.buck_error_context("string").unwrap_err()),
            &Option::<()>::None
                .compute_context(
                    |_t: Arc<SomeContext>| -> SomeContext { panic!() },
                    || "string",
                )
                .unwrap_err(),
        );
    }
}
