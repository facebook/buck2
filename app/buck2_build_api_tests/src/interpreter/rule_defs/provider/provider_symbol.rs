/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::callable::register_provider;
use buck2_build_api::interpreter::rule_defs::provider::registration::register_builtin_providers;
use buck2_interpreter_for_build::interpreter::testing::Tester;

#[test]
fn test_provider_symbol_pass() {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_provider);
    tester.additional_globals(register_builtin_providers);
    tester
        .run_starlark_bzl_test(
            r#"
MyInfo = provider(fields=["x", "y"])

def accept_provider(p: Provider):
    pass

def test():
    assert_true(isinstance(MyInfo(x=1, y=2), Provider))
    assert_true(isinstance(RunInfo([]), Provider))
    assert_false(isinstance("", Provider))

    accept_provider(MyInfo(x=1, y=2))
    accept_provider(RunInfo([]))
"#,
        )
        .unwrap();
}

#[test]
fn test_provider_symbol_fail_runtime() {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_provider);
    tester.additional_globals(register_builtin_providers);
    tester.run_starlark_bzl_test_expecting_error(
        r#"
MyInfo = provider(fields=["x", "y"])

def accept_provider(p: Provider):
    pass

def hide_type(v):
    return v

def test():
    accept_provider(hide_type(""))
"#,
        "does not match the type annotation",
    );
}

#[test]
fn test_provider_symbol_fail_compile_time() {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_provider);
    tester.additional_globals(register_builtin_providers);
    tester.run_starlark_bzl_test_expecting_error(
        r#"
MyInfo = provider(fields=["x", "y"])

def accept_provider(p: Provider):
    pass

def _compile_time_test():
    accept_provider("")

def test():
    pass
"#,
        "but got `str`",
    );
}
