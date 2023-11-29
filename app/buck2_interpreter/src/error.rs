/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

/// Wrapper for an error that was returned by starlark.
///
/// This type implements all the buck2-specific error categorization that is needed for starlark
/// errors.
pub struct BuckStarlarkError {
    e: starlark::Error,
    print_stacktrace: bool,
}

impl BuckStarlarkError {
    pub fn new(e: starlark::Error) -> Self {
        Self {
            e,
            print_stacktrace: true,
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

    fn provide<'a>(&'a self, demand: &mut buck2_error::Demand<'a>) {
        match self.e.kind() {
            starlark::ErrorKind::Other(e) => e.provide(demand),
            _ => (),
        }
    }
}

impl fmt::Debug for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.print_stacktrace {
            fmt::Debug::fmt(&self.e, f)
        } else {
            let (_, message) = self.e.get_diagnostic_and_message();
            fmt::Debug::fmt(&message, f)
        }
    }
}

impl fmt::Display for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.print_stacktrace {
            fmt::Display::fmt(&self.e, f)
        } else {
            let (_, message) = self.e.get_diagnostic_and_message();
            fmt::Debug::fmt(&message, f)
        }
    }
}
