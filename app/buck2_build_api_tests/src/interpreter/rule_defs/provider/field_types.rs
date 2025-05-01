/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::provider::callable::register_provider;
use buck2_interpreter_for_build::interpreter::testing::Tester;

fn provider_tester() -> Tester {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_provider);
    tester
}

#[test]
fn test_pass() {
    let mut tester = provider_tester();
    tester
        .run_starlark_bzl_test(
            r#"
P = provider(fields={
    "x": int,
    "y": provider_field(str),
    "z": provider_field(str | list, default = []),
})

def test():
    p0 = P(x = 1, y = "foo")
    assert_eq(1, p0.x)
    assert_eq("foo", p0.y)
    assert_eq([], p0.z)

    p1 = P(x = 1, y = "foo", z = [1])
    assert_eq(1, p1.x)
    assert_eq("foo", p1.y)
    assert_eq([1], p1.z)
"#,
        )
        .unwrap();
}

#[test]
fn test_runtime_constructor_error_on_missing_required() {
    let mut tester = provider_tester();
    tester.run_starlark_bzl_test_expecting_error(
        r#"
P = provider(fields={
    "x": provider_field(int, default = 1),
    "y": provider_field(str),
})

def p():
    return P

def test():
    p()(x = 1)
"#,
        "Missing named-only parameter `y`",
    );
}

#[test]
fn test_runtime_constructor_validates_types() {
    let mut tester = provider_tester();
    tester.run_starlark_bzl_test_expecting_error(
        r#"
P = provider(fields={
    "x": int,
})

def p():
    return P

def test():
    p()(x = "")
"#,
        "mismatches type",
    );
}

#[test]
fn test_compile_time_validate_constructor_param_types() {
    let mut tester = provider_tester();
    tester.run_starlark_bzl_test_expecting_error(
        r#"
P = provider(fields={
    "x": int,
})

def compile_time():
    return P(x = "")

def test():
    pass
"#,
        "Expected type `int` but got `str`",
    );
}

#[test]
fn test_compile_time_validate_required_param() {
    let mut tester = provider_tester();
    tester.run_starlark_bzl_test_expecting_error(
        r#"
P = provider(fields={
    "x": int,
    "y": provider_field(str),
})

def compile_time():
    return P(x = 1)

def test():
    pass
"#,
        "Missing required parameter `y`",
    );
}

#[test]
fn test_compile_time_field_types() {
    let mut tester = provider_tester();
    tester.run_starlark_bzl_test_expecting_error(
        r#"
P = provider(fields={
    "x": int,
    "y": provider_field(str),
})

def want_str(s: str):
    pass

def make_p() -> P:
    fail("should not be called")

def compile_time():
    want_str(make_p().x)

def test():
    pass
"#,
        "Expected type `str` but got `int`",
    );
}
