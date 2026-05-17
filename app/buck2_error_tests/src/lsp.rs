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
use starlark_syntax::codemap::CodeMap;
use starlark_syntax::codemap::Pos;
use starlark_syntax::codemap::Span;

fn format_lsp_diagnostics(diags: &[buck2_error::lsp::LspDiagnostic]) -> String {
    diags
        .iter()
        .map(|d| format!("{d:?}"))
        .collect::<Vec<_>>()
        .join("\n\n---\n\n")
}

#[test]
fn test_lsp_other_file_spans_visible_eval_file_spans_hidden() {
    let eval_file = CodeMap::new(
        "eval.bzl".to_owned(),
        "def NOT_visible_in_message(): pass".to_owned(),
    );
    let other_file = CodeMap::new(
        "other.bzl".to_owned(),
        "def SHOULD_be_visible(): pass".to_owned(),
    );
    let eval_span = eval_file.file_span(Span::new(Pos::new(4), Pos::new(26)));
    let other_span = other_file.file_span(Span::new(Pos::new(4), Pos::new(21)));

    let err = buck2_error::buck2_error!(buck2_error::ErrorTag::Lsp, "[visible] root cause")
        .starlark_context(
            "[visible] context from other file".to_owned(),
            Some(other_span),
            true,
        )
        .starlark_context(
            "[visible] context from eval file".to_owned(),
            Some(eval_span),
            true,
        );

    let diag = err.into_lsp_diagnostics(eval_file.filename());
    golden_test_template(
        "src/golden/test_lsp_other_file_visible.golden",
        &format_lsp_diagnostics(&diag),
    );
}

#[test]
fn test_lsp_two_eval_spans_split() {
    let eval_file = CodeMap::new(
        "eval.bzl".to_owned(),
        "first_diag()\nsecond_diag()".to_owned(),
    );
    let span1 = eval_file.file_span(Span::new(Pos::new(0), Pos::new(10)));
    let span2 = eval_file.file_span(Span::new(Pos::new(13), Pos::new(24)));

    let err = buck2_error::buck2_error!(buck2_error::ErrorTag::Lsp, "[first] root cause")
        .starlark_context(
            "[first] message for first span".to_owned(),
            Some(span1),
            true,
        )
        .context("[first] outermost context of inner span")
        .starlark_context(
            "[second] message for second span".to_owned(),
            Some(span2),
            true,
        )
        .context("[second] outermost");

    let diag = err.into_lsp_diagnostics(eval_file.filename());
    golden_test_template(
        "src/golden/test_lsp_two_eval_spans_split.golden",
        &format_lsp_diagnostics(&diag),
    );
}

#[test]
fn test_lsp_no_starlark_context() {
    let err =
        buck2_error::buck2_error!(buck2_error::ErrorTag::Lsp, "root cause").context("some context");

    let diag = err.into_lsp_diagnostics("any_file.bzl");
    golden_test_template(
        "src/golden/test_lsp_no_starlark_context.golden",
        &format_lsp_diagnostics(&diag),
    );
}

/// show_span_in_buck_output: false hides the span from buck2 output,
/// but LSP should still use it for diagnostic placement and splitting.
#[test]
fn test_lsp_show_span_false_still_splits() {
    let eval_file = CodeMap::new(
        "eval.bzl".to_owned(),
        "first_call()\nsecond_call()".to_owned(),
    );
    let span1 = eval_file.file_span(Span::new(Pos::new(0), Pos::new(10)));
    let span2 = eval_file.file_span(Span::new(Pos::new(13), Pos::new(24)));

    let err = buck2_error::buck2_error!(buck2_error::ErrorTag::Lsp, "[first] root cause")
        .starlark_context(
            "[first] label for first span".to_owned(),
            Some(span1),
            false,
        )
        .starlark_context(
            "[second] THIS SPAN SHOULD CAUSE A SECOND ERROR TO BE EMITTED FOR LSP".to_owned(),
            Some(span2),
            false,
        )
        .context("[second] outermost");

    let diag = err.into_lsp_diagnostics(eval_file.filename());
    golden_test_template(
        "src/golden/test_lsp_show_span_false_still_splits.golden",
        &format_lsp_diagnostics(&diag),
    );
}

/// show_span_in_buck_output: false on a non-eval-file SC should still
/// render the span in LSP output (the user can't see that file).
#[test]
fn test_lsp_show_span_false_other_file_still_rendered() {
    let eval_file = CodeMap::new("eval.bzl".to_owned(), "def NOT_visible(): pass".to_owned());
    let other_file = CodeMap::new(
        "other.bzl".to_owned(),
        "def SHOULD_be_visible(): pass".to_owned(),
    );
    let eval_span = eval_file.file_span(Span::new(Pos::new(4), Pos::new(15)));
    let other_span = other_file.file_span(Span::new(Pos::new(4), Pos::new(21)));

    let err = buck2_error::buck2_error!(buck2_error::ErrorTag::Lsp, "root cause")
        .starlark_context(
            "context from other file".to_owned(),
            Some(other_span),
            false,
        )
        .starlark_context("context from eval file".to_owned(), Some(eval_span), false);

    let diag = err.into_lsp_diagnostics(eval_file.filename());
    golden_test_template(
        "src/golden/test_lsp_show_span_false_other_file_rendered.golden",
        &format_lsp_diagnostics(&diag),
    );
}
