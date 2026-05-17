/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_util::golden_test_helper::golden_test_template;
use buck2_util::golden_test_helper::trim_rust_backtrace;
use starlark_syntax::call_stack::CallStack;
use starlark_syntax::codemap::CodeMap;
use starlark_syntax::codemap::Pos;
use starlark_syntax::codemap::Span;
use starlark_syntax::frame::Frame;

#[derive(Debug, buck2_error::Error)]
#[error("test error")]
#[buck2(tag = Input)]
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
        format!("{e:?}"),
        r#"context 2

Caused by:
    0: context 1
    1: test error"#,
    );
    assert_eq_no_backtrace(format!("{e:#}"), r#"context 2: context 1: test error"#);
}

#[test]
fn test_with_context_from_source() {
    #[derive(buck2_error::Error, Debug)]
    #[error("with source")]
    #[buck2(tag = Environment)]
    struct E(#[source] TestError);

    let e = buck2_error::Error::from(E(TestError)).context("context");

    assert_eq_no_backtrace(
        format!("{e:?}"),
        r#"context

Caused by:
    0: with source
    1: test error"#,
    );
    assert_eq_no_backtrace(format!("{e:#}"), r#"context: with source: test error"#);
    assert_eq_no_backtrace(format!("{e}"), r#"context"#);
}

fn example_call_stack() -> CallStack {
    CallStack {
        frames: vec![
            Frame {
                name: "frame".to_owned(),
                location: None,
            },
            Frame {
                name: "function_name".to_owned(),
                location: None,
            },
        ],
    }
}

#[test]
fn test_recover_starlark_span_through_context() {
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
    let buck = buck2_error::Error::from(starlark).context("wrapper");
    let recovered = starlark_syntax::Error::from(buck);
    let back_to_buck = buck2_error::Error::from(recovered);
    golden_test_template(
        "src/golden/test_recover_starlark_span_through_context.golden",
        trim_rust_backtrace(&format!("{back_to_buck:?}")),
    );
}

/// Non-native starlark errors (e.g. Fail, Parser) go through the
/// `error_with_starlark_context(Error::new(description, ..), sc)` path in
/// `from_starlark_impl`, placing the SC directly on the root. The root's
/// message should be absorbed as the span label in the rendered output.
#[test]
fn test_starlark_error_sc_directly_on_root() {
    let code_map = CodeMap::new(
        "build.bzl".to_owned(),
        "some_rule(\n    name = \"foo\",\n)".to_owned(),
    );
    let span = Span::new(Pos::new(0), Pos::new(9));

    let mut starlark_err = starlark_syntax::Error::new_spanned(
        starlark_syntax::ErrorKind::Fail(anyhow::anyhow!("the root cause")),
        span,
        &code_map,
    );
    starlark_err.set_call_stack(example_call_stack);

    let buck_err = buck2_error::Error::from(starlark_err);
    golden_test_template(
        "src/golden/test_starlark_error_sc_directly_on_root.golden",
        trim_rust_backtrace(&format!("{buck_err:?}")),
    );
}

/// show_span_in_buck_output: false hides the span from buck2 output.
/// This flag is set manually (not by the evaluator), so we construct it directly.
#[test]
fn test_show_span_false_hides_span_in_output() {
    let code_map = CodeMap::new("test.bzl".to_owned(), "some_call()".to_owned());
    let span = Span::new(Pos::new(0), Pos::new(9));

    let e = buck2_error::buck2_error!(buck2_error::ErrorTag::StarlarkError, "root cause")
        .starlark_context(
            "span label".to_owned(),
            Some(code_map.file_span(span)),
            false,
        )
        .context("outer context");
    golden_test_template(
        "src/golden/test_show_span_false.golden",
        trim_rust_backtrace(&format!("{e:?}")),
    );
}
