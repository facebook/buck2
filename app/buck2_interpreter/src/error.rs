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
pub struct BuckStarlarkError(starlark::Error);

impl BuckStarlarkError {
    pub fn new(e: starlark::Error) -> Self {
        Self(e)
    }

    pub fn into_inner(self) -> starlark::Error {
        self.0
    }

    pub fn inner(&self) -> &starlark::Error {
        &self.0
    }
}

impl std::error::Error for BuckStarlarkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.kind().source()
    }

    fn provide<'a>(&'a self, demand: &mut buck2_error::Demand<'a>) {
        match self.0.kind() {
            starlark::ErrorKind::Other(e) => e.provide(demand),
            _ => (),
        }
    }
}

impl fmt::Debug for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Display for BuckStarlarkError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0, f)
    }
}
