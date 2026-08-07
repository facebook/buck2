/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//! Type-related operations.

use crate::assert::Assert;
use crate::typing::tests::TypeCheck;
use crate::typing::tests::register_typecheck_globals;

#[test]
fn test_type_alias() {
    TypeCheck::new().ty("x").check(
        "type_alias",
        r#"
MyList = list[int]

def f(x: MyList):
    pass
"#,
    );
}

#[test]
fn test_incorrect_type_dot() {
    TypeCheck::new().check(
        "incorrect_type_dot",
        r#"
def foo(x: list.foo.bar):
    pass
"#,
    );
}

#[test]
fn test_function_as_type_bit_or() {
    TypeCheck::new().ty("t").check(
        "function_as_type_bit_or",
        r#"
def test():
    # This test should work even if `t` is global. There's a bug in test framework somewhere.
    t = int | str
"#,
    );
}

#[test]
fn test_function_as_type_parameterize() {
    let a = Assert::new();
    a.fail(
        r#"
def f(x: str[int]):
    pass
"#,
        "not supported",
    );
}

#[test]
fn test_starlark_value_as_type_unsupported_param() {
    let mut a = Assert::new();
    a.globals_add(register_typecheck_globals);
    a.fail(
        r#"
def f(x: MyCustomType[int]):
    pass
"#,
        "does not support type parameters",
    );
}

#[test]
fn test_list_parametrize() {
    let a = Assert::new();
    a.pass(
        r#"
def f(x: list[str]) -> list[str]:
    return x
"#,
    );
}

#[test]
fn test_set_parametrize() {
    let a = Assert::new();
    a.pass(
        r#"
def f(x: set[int]) -> set[int]:
    return x
"#,
    );
}

#[test]
fn test_list_too_many_type_params() {
    let a = Assert::new();
    a.fail(
        r#"
def f(x: list[str, int]):
    pass
"#,
        "[,] can only be applied to dict or tuple functions in type expression",
    );
}

#[test]
fn test_equality_int_float() {
    let a = Assert::new();
    // `1 == 1.0` is `True` at runtime, so comparing an `int` against a `float`
    // (in either order) must not be reported as a type error.
    a.pass(
        r#"
def f(x: int, y: float) -> bool:
    return x == y

def g(x: int, y: float) -> bool:
    return y != x

def h(x: float) -> bool:
    return x == 6
"#,
    );
}

/// Numbers nested in containers compare the same way at runtime, so the widening has
/// to recurse. All of these evaluate to `True` (verified with static typechecking
/// disabled in `test_equality_containers_are_equal_at_runtime`).
#[test]
fn test_equality_int_float_in_containers() {
    let a = Assert::new();
    a.pass(
        r#"
def f(x: list[int], y: list[float]) -> bool:
    return x == y

def g(x: dict[int, str], y: dict[float, str]) -> bool:
    return x == y

def h(x: set[int], y: set[float]) -> bool:
    return x == y

def i(x: tuple[int, ...], y: tuple[float, ...]) -> bool:
    return x == y

def j() -> bool:
    # Tuple literals produce `TyTuple::Elems` rather than `TyTuple::Of`.
    return (1, "a") == (1.0, "a")

def nested(x: list[list[int]], y: list[list[float]]) -> bool:
    return x == y
"#,
    );
}

/// Ground truth for the test above.
#[test]
fn test_equality_containers_are_equal_at_runtime() {
    let mut a = Assert::new();
    a.disable_static_typechecking();
    a.is_true("[1] == [1.0]");
    a.is_true("(1,) == (1.0,)");
    a.is_true("{1: 'a'} == {1.0: 'a'}");
}

/// Widening must not hide a genuine element-type mismatch.
#[test]
fn test_equality_container_mismatch_still_rejected() {
    let a = Assert::new();
    a.fail(
        r#"
def f(x: list[int], y: list[str]) -> bool:
    return x == y
"#,
        "Expected type `list[int]` but got `list[str]`",
    );
}

#[test]
fn test_equality_unrelated_types_still_rejected() {
    let a = Assert::new();
    a.fail(
        r#"
def f(x: int, y: str) -> bool:
    return x == y
"#,
        "Expected type `int` but got `str`",
    );
}

#[test]
fn test_equality_bool_int_still_rejected() {
    let a = Assert::new();
    // `True == 1` really is `False` in this implementation (StarlarkBool has no
    // `equals` override, and `True.unpack_num()` is `None`), so bool is correctly
    // left out of the numeric widening and the comparison stays a type error.
    a.fail(
        r#"
def f(x: bool, y: int) -> bool:
    return x == y
"#,
        "Expected type `bool` but got `int`",
    );
}
