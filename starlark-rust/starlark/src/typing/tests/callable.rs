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

//! Test for callables, but not calls.

use crate::typing::tests::TypeCheck;

#[test]
fn test_callable_with_args() {
    TypeCheck::new().check(
        "callable_with_args",
        r#"
def accept_f(x: typing.Callable[[int, str], str]):
    pass

def good_function(x: int, y: str) -> str:
    return ""

def bad_function(x: int, y: bool) -> str:
    return ""

def test():
    accept_f(good_function)
    accept_f(bad_function)
"#,
    );
}
