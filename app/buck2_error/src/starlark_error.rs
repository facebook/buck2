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
use starlark_syntax::codemap::FileSpan;

use crate::__for_macro::ContextValue;
use crate::any::recover_crate_error;
use crate::context_value::StarlarkContext;
use crate::error::ErrorKind;
use crate::source_location::SourceLocation;

impl From<crate::Error> for starlark_syntax::Error {
    fn from(e: crate::Error) -> starlark_syntax::Error {
        let error = Into::into(StarlarkErrorWrapper(e));
        let error_kind = starlark_syntax::ErrorKind::Native(error);
        starlark_syntax::Error::new_kind(error_kind)
    }
}

/// If true, we render the span / call stack in buck output.
/// Otherwise, the span data is only for LSP to fish out.
///
/// The purpose of passing "false" is to add a span for the LSP
/// diagnostic formatter.
///
// (Span is optional because some callsites only have one 99% of the time, and
// it's very inconvenient to span.map(create_starlark_context).unwrap_or(...).)
pub fn create_starlark_context(
    message: String,
    span: Option<FileSpan>,
    show_span_in_buck_output: bool,
) -> ContextValue {
    ContextValue::StarlarkError(StarlarkContext {
        error_msg: message,
        span,
        call_stack: Default::default(),
        show_span_in_buck_output,
        replaces_root_error: false,
    })
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
fn inject_starlark_context(
    e: &crate::Error,
    mut starlark_context: StarlarkContext,
) -> crate::Error {
    let mut context_stack: Vec<ContextValue> = Vec::new();
    let mut buck2_error = e.clone();

    if starlark_context.span.is_none() && starlark_context.call_stack.is_empty() {
        return e.clone();
    }

    // Not unless we reach the root in this loop.
    starlark_context.replaces_root_error = false;

    loop {
        match buck2_error.0.as_ref() {
            ErrorKind::Root(_root) => {
                // We are right next to the root.
                starlark_context.replaces_root_error = true;
                starlark_context.error_msg = format!("{buck2_error}");
                let mut err = buck2_error.context_for_starlark_backtrace(starlark_context);

                // None of these are Dyn/Typed.
                for context in context_stack.into_iter().rev() {
                    err = err.context(context);
                }

                return err;
            }
            ErrorKind::WithContext(context_value, inner) => {
                match context_value {
                    ContextValue::Dyn(_) | ContextValue::Typed(_) => {
                        // walk back up from where we got to
                        let mut inner_err = inner.clone();
                        for context in context_stack.into_iter().rev() {
                            inner_err = inner_err.context(context);
                        }

                        // A string or other context_value is popped & replaced by a StarlarkContext
                        // that describes it.
                        //
                        // XXX: we are losing the Typed nature of the context.
                        // We could modify WithContext(ctx, err) to contain a StarlarkContext
                        // directly as WithContext(ctx, starlark, err), and replace
                        // ContextValue::StarlarkError(...) with a unit variant.
                        // Thus Typed could have StarlarkContext attached.
                        //
                        starlark_context.error_msg = format!("{context_value}");

                        return inner_err
                            .clone()
                            .context_for_starlark_backtrace(starlark_context);
                    }
                    ContextValue::StarlarkError(_) => {
                        // Starlark context already attached. Don't pierce the veil.
                        // XXX: reapply context_stack? Otherwise we may lose tags!

                        // We will concatenate these by the time we render them, generally.
                        // Even if not, not the end of the world to have no message.
                        starlark_context.error_msg = String::new();
                        return buck2_error.context_for_starlark_backtrace(starlark_context);
                    }
                    ContextValue::Tags(_) | ContextValue::StringTag(_) => {
                        context_stack.push(context_value.clone())
                    }
                }

                buck2_error = inner.clone();
            }
            _ => return buck2_error,
        }
    }
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
            //
            // These two are overwritten by inject_starlark_context
            error_msg: String::new(),
            replaces_root_error: false,
        })
    } else {
        None
    };

    if let starlark_syntax::ErrorKind::Native(err) = e.kind()
        && let Some(wrapper) = err.downcast_ref::<StarlarkErrorWrapper>()
    {
        return starlark_context
            .map(|sc| inject_starlark_context(&wrapper.0, sc))
            .unwrap_or_else(|| wrapper.0.clone());
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
    let source_location = SourceLocation::new(caller_location.file())
        .with_source_line(caller_location.line())
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
            let error: anyhow::Error = Into::into(BuckStarlarkError(e, description));
            let std_err: &'_ (dyn std::error::Error + 'static) = error.as_ref();

            recover_crate_error(std_err, source_location, tag)
        }
        _ => crate::Error::new(description, tag, source_location, None),
    };

    starlark_context
        .map(|sc| inject_starlark_context(&crate_error, sc))
        .unwrap_or(crate_error)
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
    use std::sync::Arc;

    use allocative::Allocative;
    use starlark_syntax::call_stack::CallStack;
    use starlark_syntax::frame::Frame;

    use crate::any::ProvidableMetadata;
    use crate::buck2_error;
    use crate::context_value::StarlarkContext;
    use crate::source_location::SourceLocation;
    use crate::starlark_error::inject_starlark_context;

    #[derive(Debug, Allocative, derive_more::Display)]
    struct FullMetadataError;

    impl From<FullMetadataError> for crate::Error {
        #[cold]
        fn from(value: FullMetadataError) -> Self {
            let error = anyhow::Error::from(value);
            let source_location = SourceLocation::new(file!());
            crate::any::recover_crate_error(error.as_ref(), source_location, crate::ErrorTag::Tier0)
        }
    }

    impl std::error::Error for FullMetadataError {
        fn provide<'a>(&'a self, request: &mut Request<'a>) {
            request.provide_value(ProvidableMetadata {
                action_error: None,
                source_location: SourceLocation::new(file!()).with_type_name("FullMetadataError"),
                tags: vec![
                    crate::ErrorTag::WatchmanTimeout,
                    crate::ErrorTag::StarlarkNativeInput,
                    crate::ErrorTag::StarlarkFail,
                ],
                string_tags: vec![],
            });
        }
    }

    impl crate::TypedContext for FullMetadataError {
        fn eq(&self, _other: &dyn crate::TypedContext) -> bool {
            true
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
    fn test_metadata_roundtrip_with_anyhow() {
        // Tests buck2_error->anyhow->starlark->buck2_error
        let e = crate::Error::from(FullMetadataError);
        let e = e.context("test context 123");
        let e: anyhow::Error = e.into();
        let e: starlark_syntax::Error = e.into();
        let e: crate::Error = e.into();

        assert_eq!(e.get_tier(), Some(crate::Tier::Tier0));
        assert!(format!("{e:?}").contains("test context 123"));
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/starlark_error.rs::FullMetadataError",
        );
        assert_eq!(
            &e.tags(),
            &[
                crate::ErrorTag::StarlarkFail,
                crate::ErrorTag::StarlarkNativeInput,
                crate::ErrorTag::Tier0,
                crate::ErrorTag::WatchmanTimeout
            ]
        );
    }

    #[test]
    fn test_pop_dyn_error_from_context() {
        let sc_message = "This should be ignored";
        let context_error = "Some error message that's getting popped";
        let context_key = "Some context key";
        let error_tag = crate::ErrorTag::IoWindowsSharingViolation;
        let starlark_context = StarlarkContext {
            call_stack: example_call_stack(),
            error_msg: sc_message.to_owned(),
            span: None,
            replaces_root_error: false,
            show_span_in_buck_output: true,
        };

        let e = crate::Error::from(FullMetadataError);
        let e = e.context(context_error);
        let e = e.tag([error_tag]);
        let e = e.string_tag(context_key);

        let context_popped = inject_starlark_context(&e, starlark_context);

        let debug_out = format!("{context_popped:?}");
        assert_eq!(debug_out.matches(context_error).count(), 1);
        assert!(!debug_out.contains(sc_message));
        assert!(context_popped.tags().contains(&error_tag));
        assert!(context_popped.category_key().ends_with(context_key));
        assert!(
            context_popped
                .find_starlark_context()
                .is_some_and(|x| x.error_msg == context_error)
        );
    }

    #[test]
    fn test_pop_base_error_from_context() {
        let base_error = "Some base error";
        let starlark_error = "This should be ignored";
        let starlark_call_stack = example_call_stack();
        let starlark_context = StarlarkContext {
            call_stack: starlark_call_stack.clone(),
            error_msg: starlark_error.to_owned(),
            span: None,
            replaces_root_error: false,
            show_span_in_buck_output: true,
        };

        let e = buck2_error!(crate::ErrorTag::StarlarkError, "{}", base_error);

        let injected = inject_starlark_context(&e, starlark_context);

        let debug_out = format!("{injected:?}");
        assert_eq!(debug_out.matches(base_error).count(), 1);
        assert!(debug_out.contains(&starlark_call_stack.to_string()));
        assert!(!injected.to_string().contains(starlark_error));
        assert!(injected.find_starlark_context().is_some());
    }

    #[test]
    fn test_conversion_with_typed_context() {
        let base_error = "Some base error";
        let e = buck2_error!(crate::ErrorTag::StarlarkError, "{}", base_error);
        let e = e.compute_context(
            |_: Arc<FullMetadataError>| FullMetadataError,
            || FullMetadataError,
        );

        let starlark_call_stack = example_call_stack();
        let starlark_error_msg = "This should be ignored";
        let starlark_context = StarlarkContext {
            call_stack: starlark_call_stack.clone(),
            error_msg: starlark_error_msg.to_owned(),
            span: None,
            replaces_root_error: false,
            show_span_in_buck_output: true,
        };

        let injected = inject_starlark_context(&e, starlark_context);
        let debug_out = format!("{injected:?}");

        assert!(debug_out.contains(&starlark_call_stack.to_string()));
        assert!(!debug_out.contains(starlark_error_msg));
        assert!(injected.find_starlark_context().is_some());
        // Root error shouldn't be lost
        assert_eq!(debug_out.matches(base_error).count(), 1);
    }

    #[test]
    fn test_format_starlark_error_via_buck() {
        use starlark_syntax::codemap::CodeMap;
        use starlark_syntax::codemap::Pos;
        use starlark_syntax::codemap::Span;
        let code_map = CodeMap::new(
            "test.bzl".to_owned(),
            "# invalid\ndef and(): pass".to_owned(),
        );
        let span = Span::new(Pos::new(14), Pos::new(17));
        let starlark = starlark_syntax::Error::new_spanned(
            starlark_syntax::ErrorKind::Native(anyhow::format_err!("test_recover_starlark_span")),
            span,
            &code_map,
        );
        eprintln!("starlark: {:#?}", starlark);
        let expect_debug = starlark.to_string().trim().to_owned();
        let expect_display = starlark.to_string().trim().to_owned();
        let buck = crate::Error::from(starlark);
        eprintln!("buck: {:?}", buck);
        eprintln!("buck: {:#?}", buck);

        assert_eq!(
            format!("{:?}", buck).trim(),
            expect_debug,
            "(Debug): Buck error should format the same as the original"
        );

        // Check we haven't duplicated the source printout
        assert_eq!(
            buck.to_string().trim(),
            expect_display,
            "(Display): Buck error should format the same as the original"
        );
    }
}
