/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Errors from buck's starlark debugger
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
pub(crate) enum StarlarkDebuggerError {
    #[error("starlark debugger has not yet implemented this functionality")]
    Unimplemented,
    #[error("another debug-attach process is already attached")]
    DebuggerAlreadyAttached,
}

/// Internal errors from buck's starlark debugger
#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Tier0)]
pub(crate) enum StarlarkDebuggerInternalError {
    #[error("Internal error: debbugger server shutdown unexpectedly")]
    UnexpectedDebuggerShutdown,
    #[error("Internal error: debug controller already initialized with Evaluator")]
    EvalWrapperAlreadyUsed,
}
