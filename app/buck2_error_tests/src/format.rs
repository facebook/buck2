/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::golden_test_helper::trim_rust_backtrace;

#[derive(Debug, thiserror::Error)]
#[error("test error")]
struct TestError;

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
    assert_eq_no_backtrace(format!("{:#}", e), r#"context 2: context 1: test error"#);
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
    assert_eq_no_backtrace(format!("{:#}", e), format!("{:#}", e2));

    let e3 = buck2_error::Error::from(e2);
    assert_eq_no_backtrace(format!("{}", e), format!("{}", e3));
    assert_eq_no_backtrace(format!("{:?}", e), format!("{:?}", e3));
    assert_eq_no_backtrace(format!("{:#}", e), format!("{:#}", e3));
}

#[test]
fn test_with_context_from_source() {
    #[derive(buck2_error::Error, Debug)]
    #[error("with source")]
    struct E(#[source] TestError);

    let e = buck2_error::Error::from(E(TestError)).context("context");

    assert_eq_no_backtrace(
        format!("{:?}", e),
        r#"context

Caused by:
    0: with source
    1: test error"#,
    );
    assert_eq_no_backtrace(format!("{:#}", e), r#"context: with source: test error"#);
    assert_eq_no_backtrace(format!("{}", e), r#"context"#);
}
