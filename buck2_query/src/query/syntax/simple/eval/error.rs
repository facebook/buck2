/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implementation of the cli and query_* attr query language.

use buck2_core::fs::paths::AbsPathBuf;
use buck2_query_parser::spanned::Spanned;
use thiserror::Error;

/// While this is a std Error type, we generally don't use it directly. It's instead wrapped in a Spanned and can be converted to a normal error
/// with QueryError::convert_error (which will resolve the spans to context messages).
#[derive(Debug, Error)]
pub enum QueryError {
    #[error("unknown function `{0}`")]
    UnknownFunction(String),
    #[error("binary op `{0}` unsupported in this context")]
    UnsupportedBinaryOp(String),
    #[error("expected a literal, got value of type `{actual}`")]
    ExpectedLiteral { actual: &'static str },
    #[error("expected value of type `{expected}`, got `{actual}`")]
    InvalidType {
        expected: &'static str,
        actual: &'static str,
    },
    #[error("too many args. function `{function}` accepts maximum {max} args, got {actual}")]
    TooManyArgs {
        function: String,
        max: usize,
        actual: usize,
    },
    #[error("too few args. function `{function}` requires at least {min} args, got {actual}")]
    TooFewArgs {
        function: String,
        min: usize,
        actual: usize,
    },
    #[error("function `{0}` is not implemented yet")]
    FunctionUnimplemented(&'static str),
    #[error("Argument `{1}` to `{0}` is not yet supported in buck2")]
    ArgNotYetSupported(String, String),
    #[error("Invalid traversal depth `{0}`")]
    InvalidDepth(i32),
    #[error("File literal `{1}` not within the project root `{}`", .0.display())]
    FileLiteralNotInProject(AbsPathBuf, String),
    #[error("query function {0} not available in this context")]
    NotAvailableInContext(&'static str),
    /// Used to propagate up an inner error. The inner span will mark where the inner error was (which itself may be the
    /// propagation of another error). This error will end up in a Spanned that indicates where this error (the propagation) occurs.
    /// Since QueryError has an impl for From<Spanned<QueryError>>, just propagating inner eval errors via `?` will hit this case (and
    /// so will actually end up not just propagating the inner error but wrapping it in this and adding an additional Spanned to it).
    #[error("{0}")]
    Inner(Box<Spanned<QueryError>>),
    /// Used to propagate an inner error. Generally this would happen when we are calling something outside the evaluation
    /// (ex. target parsing). That inner error won't have span information and so the innermost span information will be the one
    /// attached to this error.
    #[error("{0}")]
    Anyhow(anyhow::Error),
}

impl From<anyhow::Error> for QueryError {
    fn from(err: anyhow::Error) -> Self {
        QueryError::Anyhow(err)
    }
}

impl<'a> From<Spanned<QueryError>> for QueryError {
    fn from(value: Spanned<QueryError>) -> Self {
        QueryError::Inner(box value)
    }
}

impl QueryError {
    pub fn drop_spans(err: Spanned<Self>) -> anyhow::Error {
        match err.value {
            Self::Anyhow(inner) => inner,
            Self::Inner(inner) => Self::drop_spans(*inner),
            e => {
                // TODO(cjhopman): This is going to drop the backtrace attached to the error, we should figure
                // out how to keep that.
                anyhow::anyhow!(e)
            }
        }
    }
    pub fn convert_error(err: Spanned<Self>, input: &str) -> anyhow::Error {
        let context = err.get_err_context(input);

        match err.value {
            Self::Anyhow(inner) => inner.context(format!("when evaluating expression:{}", context)),
            Self::Inner(inner) => Self::convert_error(*inner, input)
                .context(format!("when evaluating expression:{}", context)),
            e => {
                // TODO(cjhopman): This is going to drop the backtrace attached to the error, we should figure
                // out how to keep that.
                anyhow::anyhow!("{}:{}", e, context)
            }
        }
    }
}
