/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use crate::assert;

mod pass {
    use super::*;

    #[test]
    fn basic() {
        assert::is_true(
            r#"
x = "a"
f"{x}b" == "ab"
"#,
        );

        assert::is_true(
            r#"
x = "a"
f"{ x }b" == "ab"
"#,
        );

        assert::is_true(
            r#"
x = "a"
f"{x}b{x}" == "aba"
"#,
        );
    }

    #[test]
    fn escape() {
        assert::is_true(
            r#"
x = "a"
f"{{}}{x}b{{x}}" == "{}ab{x}"
"#,
        );
    }

    #[test]
    fn function_parameter() {
        assert::is_true(
            r#"
def f(x):
  return f"q{x}"

f("1") == "q1"
"#,
        );
    }

    #[test]
    fn multiple() {
        assert::is_true(
            r#"
x = "x"
y = "y"
f"{x}{y}" == "xy"
"#,
        );

        assert::is_true(
            r#"
x = "x"
y = "y"
f"{x}{y}{x}" == "xyx"
"#,
        );
    }

    #[test]
    fn tuple() {
        assert::is_true(
            r#"
x = ("x",)
f"{x}" == '("x",)'
"#,
        );
    }
}

mod fail {
    use super::*;

    #[test]
    fn undef() {
        assert::fail("f'{x}b'", "Variable `x` not found");
    }

    #[test]
    fn invalid_format() {
        assert::fail("f'{x'", "Invalid format");
        assert::fail("f'{x}}'", "Invalid format");
    }

    #[test]
    fn expression() {
        assert::fail("f'{123}'", "Not a valid identifier: `123`");
        assert::fail("f'{x[123]}'", "Not a valid identifier: `x[123]`");
    }
}
