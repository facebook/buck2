/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use crate::error::ErrorKind;

/// We currently implement formatting in the laziest way possible - we convert to an equivalent
/// `anyhow::Error` and format that.
///
/// In the long term, this is not what we want to do. Writing our own error formatter is not that
/// hard and will give us a huge amount of flexibility. However, the goal right now is to get large
/// amounts of `anyhow` compatibility with minimal work, and this achieves that.
fn into_anyhow_for_format(mut error: &crate::Error) -> anyhow::Error {
    let mut context_stack = Vec::new();

    let root = loop {
        match error.0.as_ref() {
            ErrorKind::Root(root) => break root,
            ErrorKind::WithContext(context, inner) => {
                context_stack.push(context.clone());
                error = inner;
            }
            ErrorKind::Emitted(inner) => {
                error = inner;
            }
        }
    };

    let mut out: anyhow::Error = root.into_anyhow_for_format();
    for context in context_stack.into_iter().rev() {
        out = out.context(context);
    }
    out
}

impl fmt::Debug for crate::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&into_anyhow_for_format(self), f)
    }
}

impl fmt::Display for crate::Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&into_anyhow_for_format(self), f)
    }
}

#[cfg(test)]
mod tests {
    use crate as buck2_error;

    #[derive(Debug, thiserror::Error)]
    #[error("test error")]
    struct TestError;

    fn trim_rust_backtrace(error: &str) -> &str {
        match error.find("\nStack backtrace:") {
            Some(pos) => error[..pos].trim_end(),
            None => error.trim_end(),
        }
    }

    fn assert_eq_no_backtrace<T: AsRef<str>, U: AsRef<str>>(a: T, b: U) {
        assert_eq!(
            trim_rust_backtrace(a.as_ref()),
            trim_rust_backtrace(b.as_ref())
        );
    }

    #[test]
    fn test_shows_context() {
        let e = buck2_error::Error::from(TestError)
            .context("context 1")
            .context("context 2");
        assert_eq_no_backtrace(
            format!("{:?}", e),
            r#"context 2

Caused by:
    0: context 1
    1: test error"#,
        );
    }

    #[test]
    fn test_shows_anyhow_context() {
        // This context can't be understood by `buck2_error`
        let e = anyhow::Error::from(TestError).context("context 1");
        let e = buck2_error::Error::from(e).context("context 2");
        assert_eq_no_backtrace(
            format!("{:?}", e),
            r#"context 2

Caused by:
    0: context 1
    1: test error"#,
        );
    }

    #[test]
    fn test_after_anyhow_conversion() {
        let e = buck2_error::Error::from(TestError).context("context");
        let e2 = anyhow::Error::from(e.clone());
        assert_eq_no_backtrace(format!("{}", e), format!("{}", e2));
        assert_eq_no_backtrace(format!("{:?}", e), format!("{:?}", e2));

        let e3 = buck2_error::Error::from(e2);
        assert_eq_no_backtrace(format!("{}", e), format!("{}", e3));
        assert_eq_no_backtrace(format!("{:?}", e), format!("{:?}", e3));
    }
}
