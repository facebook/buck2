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
///
/// TODO(JakobDegen): Replace the inner error with `starlark::Error`
pub struct BuckStarlarkError(anyhow::Error);

impl BuckStarlarkError {
    pub fn new(e: anyhow::Error) -> Self {
        Self(e)
    }

    pub fn into_inner(self) -> anyhow::Error {
        self.0
    }

    pub fn inner(&self) -> &anyhow::Error {
        &self.0
    }
}

impl std::error::Error for BuckStarlarkError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.source()
    }

    fn provide<'a>(&'a self, demand: &mut buck2_error::Demand<'a>) {
        self.0.provide(demand);
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
