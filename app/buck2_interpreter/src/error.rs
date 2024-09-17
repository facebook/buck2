/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

/// Whether or not to mark a starlark error as an input/user error.
#[derive(PartialEq)]
pub enum OtherErrorHandling {
    InputError,
    Unknown,
}

/// Wrapper for an error that was returned by starlark.
///
/// This type implements all the buck2-specific error categorization that is needed for starlark
/// errors.
pub struct BuckStarlarkError {
    e: starlark::Error,
    print_stacktrace: bool,
    error_handling: OtherErrorHandling,
}

impl BuckStarlarkError {
    pub fn new(e: starlark::Error, error_handling: OtherErrorHandling) -> Self {
        Self {
            e,
            print_stacktrace: true,
            error_handling,
        }
    }

    pub fn set_print_stacktrace(&mut self, print_stacktrace: bool) {
        self.print_stacktrace = print_stacktrace;
    }

    pub fn into_inner(self) -> starlark::Error {
        self.e
    }

    pub fn inner(&self) -> &starlark::Error {
        &self.e
    }
}

impl std::error::Error for BuckStarlarkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.e.kind().source()
    }

    fn provide<'a>(&'a self, request: &mut std::error::Request<'a>) {
        match self.e.kind() {
            starlark::ErrorKind::Other(e) => {
                // Try to get the underlying error's metadata. If there's nothing here, then the
                // rest of this function will provide a fallback value
                e.provide(request);
            }
            _ => (),
        }

        let category = match self.e.kind() {
            starlark::ErrorKind::Fail(_)
            | starlark::ErrorKind::StackOverflow(_)
            | starlark::ErrorKind::Internal(_)
            | starlark::ErrorKind::Value(_) => Some(buck2_error::Tier::Input),
            starlark::ErrorKind::Function(_) => Some(buck2_error::Tier::Input),
            starlark::ErrorKind::Scope(_) => Some(buck2_error::Tier::Input),
            starlark::ErrorKind::Parser(_) => Some(buck2_error::Tier::Input),
            starlark::ErrorKind::Other(_)
                if self.error_handling == OtherErrorHandling::InputError =>
            {
                Some(buck2_error::Tier::Input)
            }
            _ => None,
        };
        let tags = match self.e.kind() {
            starlark::ErrorKind::Fail(_) => &[buck2_error::ErrorTag::StarlarkFail][..],
            starlark::ErrorKind::StackOverflow(_) => {
                &[buck2_error::ErrorTag::StarlarkStackOverflow][..]
            }
            _ => &[][..],
        };
        let variant_name = match self.e.kind() {
            starlark::ErrorKind::Fail(_) => "BuckStarlarkError::Fail",
            starlark::ErrorKind::StackOverflow(_) => "BuckStarlarkError::StackOverflow",
            starlark::ErrorKind::Internal(_) => "BuckStarlarkError::Internal",
            starlark::ErrorKind::Value(_) => "BuckStarlarkError::Value",
            starlark::ErrorKind::Function(_) => "BuckStarlarkError::Function",
            starlark::ErrorKind::Scope(_) => "BuckStarlarkError::Scope",
            starlark::ErrorKind::Parser(_) => "BuckStarlarkError::Lexer",
            _ => "BuckStarlarkError",
        };
        buck2_error::provide_metadata(
            request,
            category,
            None, /* typ */
            tags.iter()
                .copied()
                .chain([buck2_error::ErrorTag::AnyStarlarkEvaluation]),
            std::file!(),
            Some(variant_name),
            None, /* action error */
        );
    }
}

impl fmt::Debug for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.print_stacktrace {
            fmt::Debug::fmt(&self.e, f)
        } else {
            fmt::Debug::fmt(&self.e.without_diagnostic(), f)
        }
    }
}

impl fmt::Display for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.print_stacktrace {
            fmt::Display::fmt(&self.e, f)
        } else {
            fmt::Debug::fmt(&self.e.without_diagnostic(), f)
        }
    }
}
