/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Starlark::Error <-> buck2_error::Error conversion implementation.

use std::fmt;

use ref_cast::RefCast;

use crate::any::from_any_with_tag_and_source_location;
use crate::context_value::StarlarkContext;
use crate::source_location::SourceLocation;

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

impl From<starlark_syntax::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(e: starlark_syntax::Error) -> Self {
        from_starlark_impl(e, NativeErrorHandling::InputError, false)
    }
}

#[cold]
pub fn from_starlark_with_options(
    e: starlark_syntax::Error,
    error_handling: NativeErrorHandling,
    skip_stacktrace: bool,
) -> crate::Error {
    from_starlark_impl(e, error_handling, skip_stacktrace)
}

// If the top context is a string context, then it should be popped off as otherwise
// the error will be duplicated since the top level error is used to build the stacktrace.
fn error_with_starlark_context(e: crate::Error, starlark_context: StarlarkContext) -> crate::Error {
    if starlark_context.span.is_none() && starlark_context.call_stack.is_empty() {
        return e;
    }

    e.context_for_starlark_backtrace(starlark_context)
}

#[track_caller]
fn from_starlark_impl(
    e: starlark_syntax::Error,
    error_handling: NativeErrorHandling,
    skip_stacktrace: bool,
) -> crate::Error {
    let starlark_context = if e.has_diagnostic() && !skip_stacktrace {
        Some(StarlarkContext {
            call_stack: e.call_stack().clone(),
            span: e.span().cloned(),
            show_span_in_buck_output: true,
        })
    } else {
        None
    };

    if let starlark_syntax::ErrorKind::Native(err) = e.kind()
        && let Some(inner) = err
            .downcast_ref::<StarlarkErrorWrapper>()
            .map(|StarlarkErrorWrapper(e)| e.clone())
    {
        return match starlark_context {
            Some(sc) => error_with_starlark_context(inner, sc),
            None => inner,
        };
    }

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
    let caller_location = std::panic::Location::caller();
    let source_location = SourceLocation::new(caller_location.file(), caller_location.line())
        .with_type_name(variant_name);
    let description = format!("{}", e.without_diagnostic());

    let crate_error = match e.into_kind() {
        starlark_syntax::ErrorKind::Fail(e)
        | starlark_syntax::ErrorKind::StackOverflow(e)
        | starlark_syntax::ErrorKind::Internal(e)
        | starlark_syntax::ErrorKind::Value(e)
        | starlark_syntax::ErrorKind::Function(e)
        | starlark_syntax::ErrorKind::Scope(e)
        | starlark_syntax::ErrorKind::Parser(e)
        | starlark_syntax::ErrorKind::Other(e)
        | starlark_syntax::ErrorKind::Freeze(e)
        | starlark_syntax::ErrorKind::Native(e) => {
            let error = BuckStarlarkError(e, description);

            from_any_with_tag_and_source_location(&error, source_location, tag)
        }
        _ => crate::Error::new(description, tag, source_location, None),
    };

    match starlark_context {
        Some(sc) => error_with_starlark_context(crate_error, sc),
        None => crate_error,
    }
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
    use std::sync::Arc;

    use allocative::Allocative;
    use starlark_syntax::call_stack::CallStack;
    use starlark_syntax::frame::Frame;

    use crate as buck2_error;
    use crate::buck2_error;
    use crate::context_value::StarlarkContext;
    use crate::starlark_error::error_with_starlark_context;

    #[derive(buck2_error::Error, Debug, Allocative)]
    #[error("FullMetadataError")]
    #[buck2(tag = crate::ErrorTag::Tier0)]
    struct FullMetadataError;

    impl crate::TypedContext for FullMetadataError {
        fn eq(&self, _other: &dyn crate::TypedContext) -> bool {
            true
        }

        fn display(&self) -> Option<String> {
            Some("FullMetadataError".to_owned())
        }
    }

    fn example_call_stack() -> CallStack {
        CallStack {
            frames: vec![Frame {
                name: "frame".to_owned(),
                location: None,
            }],
        }
    }

    #[test]
    fn test_roundtrip_starlark() {
        // Tests buck2_error->starlark->buck2_error
        let e = crate::Error::from(FullMetadataError).context("context 1");
        let e2: crate::Error = starlark_syntax::Error::from(e.clone()).into();
        crate::Error::check_equal(&e, &e2);
    }

    #[test]
    fn test_error_with_starlark_context_preserves_tags() {
        let context_error = "Some error message";
        let context_key = "Some context key";
        let error_tag = crate::ErrorTag::IoWindowsSharingViolation;
        let starlark_context = StarlarkContext {
            call_stack: example_call_stack(),
            span: None,
            show_span_in_buck_output: true,
        };

        let e = crate::Error::from(FullMetadataError);
        let e = e.context(context_error);
        let e = e.tag([error_tag]);
        let e = e.string_tag(context_key);

        let context_popped = error_with_starlark_context(e, starlark_context);

        let debug_out = format!("{context_popped:?}");
        assert_eq!(debug_out.matches(context_error).count(), 1);
        assert!(context_popped.tags().contains(&error_tag));
        assert!(context_popped.category_key().ends_with(context_key));
        assert!(context_popped.find_starlark_context().is_some());
    }

    #[test]
    fn test_pop_base_error_from_context() {
        let base_error = "Some base error";
        let starlark_error = "This should be ignored";
        let starlark_call_stack = example_call_stack();
        let starlark_context = StarlarkContext {
            call_stack: starlark_call_stack.clone(),
            span: None,
            show_span_in_buck_output: true,
        };

        let e = buck2_error!(crate::ErrorTag::StarlarkError, "{}", base_error)
            .context(starlark_error.to_owned());

        let injected = error_with_starlark_context(e, starlark_context);

        let debug_out = format!("{injected:?}");
        eprintln!("{debug_out}");
        assert_eq!(debug_out.matches(base_error).count(), 1);
        assert!(injected.tags().contains(&crate::ErrorTag::StarlarkError));
        assert!(injected.find_starlark_context().is_some());
    }

    #[test]
    fn test_typed_context_preserved_through_starlark() {
        let base_error = "Some base error";
        let span_label = "error during evaluation";
        let e = buck2_error!(crate::ErrorTag::StarlarkError, "{}", base_error);
        let e = e
            .compute_context(
                |_: Arc<FullMetadataError>| FullMetadataError,
                || FullMetadataError,
            )
            .context(span_label.to_owned());

        let starlark_call_stack = example_call_stack();
        let starlark_context = StarlarkContext {
            call_stack: starlark_call_stack.clone(),
            span: None,
            show_span_in_buck_output: true,
        };

        let injected = error_with_starlark_context(e, starlark_context);
        let debug_out = format!("{injected:?}");
        eprintln!("{debug_out}");

        assert!(debug_out.contains(&starlark_call_stack.to_string()));
        // span_label is absorbed into the span display as the label
        assert_eq!(debug_out.matches(span_label).count(), 1);
        assert!(injected.find_starlark_context().is_some());
        // Root error shouldn't be lost
        assert_eq!(debug_out.matches(base_error).count(), 1);
    }
}
