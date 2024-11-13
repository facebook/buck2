/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark::Error <-> buck2_error::Error conversion implementation.

use std::fmt;
use std::sync::Arc;

use ref_cast::RefCast;

use crate::any::recover_crate_error;
use crate::context_value::StarlarkContext;
use crate::error::ErrorKind;
use crate::root::ErrorRoot;

impl From<crate::Error> for starlark_syntax::Error {
    fn from(e: crate::Error) -> starlark_syntax::Error {
        let error = Into::into(StarlarkErrorWrapper(e));
        let error_kind = starlark_syntax::ErrorKind::Native(error);
        starlark_syntax::Error::new_kind(error_kind)
    }
}

/// Whether or not to mark a starlark error as an input/user error.
/// TODO(minglunli): This is used as an intermediate step so Native errors are categorized.
/// This is not 100% accurate and should be remove once Native errors are categorized properly
#[derive(PartialEq)]
pub enum NativeErrorHandling {
    InputError,
    Unknown,
}

// Need to do this for now instead `impl From<starlark::Error> for crate::Error`
// Otherwise it will conflict with `From<T> for crate::Error` impl in any.rs
// This will change once the `anyhow` migration is complete at which point we can update this
#[cold]
pub fn from_starlark(e: starlark_syntax::Error) -> crate::Error {
    from_starlark_impl(e, NativeErrorHandling::InputError, false)
}

#[cold]
pub fn from_starlark_with_options(
    e: starlark_syntax::Error,
    error_handling: NativeErrorHandling,
    skip_stacktrace: bool,
) -> crate::Error {
    from_starlark_impl(e, error_handling, skip_stacktrace)
}

fn from_starlark_impl(
    e: starlark_syntax::Error,
    error_handling: NativeErrorHandling,
    skip_stacktrace: bool,
) -> crate::Error {
    if let starlark_syntax::ErrorKind::Native(err) = e.kind() {
        if let Some(wrapper) = err.downcast_ref::<StarlarkErrorWrapper>() {
            if !e.has_diagnostic() {
                return wrapper.0.clone();
            }

            let starlark_context = StarlarkContext {
                call_stack: format!("{}", e.call_stack()),
                error_msg: format!("{}", e.without_diagnostic()),
                span: e.span().cloned(),
            };

            let buck2_err = wrapper
                .0
                .clone()
                .context_for_starlark_backtrace(starlark_context);

            return buck2_err;
        }
    };

    let tag = match e.kind() {
        starlark_syntax::ErrorKind::Fail(_) => crate::ErrorTag::StarlarkFail,
        starlark_syntax::ErrorKind::StackOverflow(_) => crate::ErrorTag::StarlarkStackOverflow,
        starlark_syntax::ErrorKind::Value(_) => crate::ErrorTag::StarlarkValue,
        starlark_syntax::ErrorKind::Function(_) => crate::ErrorTag::StarlarkFunction,
        starlark_syntax::ErrorKind::Scope(_) => crate::ErrorTag::StarlarkScope,
        starlark_syntax::ErrorKind::Parser(_) => crate::ErrorTag::StarlarkParser,
        starlark_syntax::ErrorKind::Internal(_) => crate::ErrorTag::StarlarkInternal,
        starlark_syntax::ErrorKind::Native(_) | starlark_syntax::ErrorKind::Other(_)
            if error_handling == NativeErrorHandling::InputError =>
        {
            crate::ErrorTag::StarlarkNativeInput
        }
        _ => crate::ErrorTag::StarlarkError,
    };

    let variant_name = match e.kind() {
        starlark_syntax::ErrorKind::Fail(_) => "StarlarkError::Fail",
        starlark_syntax::ErrorKind::StackOverflow(_) => "StarlarkError::StackOverflow",
        starlark_syntax::ErrorKind::Internal(_) => "StarlarkError::Internal",
        starlark_syntax::ErrorKind::Value(_) => "StarlarkError::Value",
        starlark_syntax::ErrorKind::Function(_) => "StarlarkError::Function",
        starlark_syntax::ErrorKind::Scope(_) => "StarlarkError::Scope",
        starlark_syntax::ErrorKind::Parser(_) => "StarlarkError::Parser",
        starlark_syntax::ErrorKind::Native(_) => "StarlarkError::Native",
        _ => "StarlarkError",
    };
    let source_location = crate::source_location::from_file(std::file!(), Some(variant_name));
    let description = if skip_stacktrace {
        format!("{}", e.without_diagnostic())
    } else {
        format!("{}", e)
    };

    match e.into_kind() {
        starlark_syntax::ErrorKind::Fail(e)
        | starlark_syntax::ErrorKind::StackOverflow(e)
        | starlark_syntax::ErrorKind::Internal(e)
        | starlark_syntax::ErrorKind::Value(e)
        | starlark_syntax::ErrorKind::Function(e)
        | starlark_syntax::ErrorKind::Scope(e)
        | starlark_syntax::ErrorKind::Parser(e)
        | starlark_syntax::ErrorKind::Other(e)
        | starlark_syntax::ErrorKind::Native(e) => {
            let error: anyhow::Error = Into::into(BuckStarlarkError(e, description));
            let std_err: &'_ (dyn std::error::Error + 'static) = error.as_ref();

            recover_crate_error(std_err, source_location)
        }
        _ => crate::Error(Arc::new(ErrorKind::Root(Box::new(ErrorRoot::new(
            description,
            source_location,
            None,
        ))))),
    }
    .tag(vec![tag])
}

pub(crate) struct BuckStarlarkError(pub(crate) anyhow::Error, String);

impl fmt::Debug for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl fmt::Display for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.1)
    }
}

impl std::error::Error for BuckStarlarkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }

    fn provide<'a>(&'a self, request: &mut std::error::Request<'a>) {
        self.0.provide(request);
    }
}

#[derive(derive_more::Display, RefCast)]
#[repr(transparent)]
struct StarlarkErrorWrapper(crate::Error);

impl fmt::Debug for StarlarkErrorWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl std::error::Error for StarlarkErrorWrapper {}

#[cfg(test)]
mod tests {
    use std::error::Request;

    use super::from_starlark;
    use crate::any::ProvidableMetadata;

    #[derive(Debug, derive_more::Display)]
    struct FullMetadataError;

    impl std::error::Error for FullMetadataError {
        fn provide<'a>(&'a self, request: &mut Request<'a>) {
            request.provide_value(ProvidableMetadata {
                action_error: None,
                source_file: file!(),
                source_location_extra: Some("FullMetadataError"),
                tags: vec![
                    crate::ErrorTag::WatchmanTimeout,
                    crate::ErrorTag::StarlarkNativeInput,
                    crate::ErrorTag::StarlarkFail,
                ],
                category: None,
            });
        }
    }

    #[test]
    fn test_roundtrip_starlark() {
        // Tests buck2_error->starlark->buck2_error
        let e = crate::Error::new(FullMetadataError).context("context 1");
        let e2 = from_starlark(starlark_syntax::Error::from(e.clone()));
        crate::Error::check_equal(&e, &e2);
    }

    #[test]
    fn test_metadata_roundtrip_with_anyhow() {
        // Tests buck2_error->anyhow->starlark->buck2_error
        let e = crate::Error::new(FullMetadataError);
        let e = e.context("test context 123");
        let e: anyhow::Error = e.into();
        let e: starlark_syntax::Error = e.into();
        let e = from_starlark(e);

        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
        assert!(format!("{:?}", e).contains("test context 123"));
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/starlark_error.rs::FullMetadataError")
        );
        assert_eq!(
            &e.tags(),
            &[
                crate::ErrorTag::StarlarkFail,
                crate::ErrorTag::StarlarkNativeInput,
                crate::ErrorTag::WatchmanTimeout
            ]
        );
    }
}
