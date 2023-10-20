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

//! Implementation of `call_stack` function.

use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::eval::Evaluator;

#[starlark_module]
pub(crate) fn global(builder: &mut GlobalsBuilder) {
    /// Get a textual representation of the call stack.
    ///
    /// This is intended only for debugging purposes to display to a human and
    /// should not be considered stable or parseable.
    ///
    /// strip_frames will pop N frames from the top of the call stack, which can
    /// be useful to hide non-interesting lines - for example, strip_frames=1
    /// will hide the call to and location of `call_stack()` itself.
    fn call_stack(
        #[starlark(require=named, default = 0)] strip_frames: u32,
        eval: &mut Evaluator,
    ) -> anyhow::Result<String> {
        let mut stack = eval.call_stack();
        stack
            .frames
            .truncate(stack.frames.len().saturating_sub(strip_frames as usize));
        Ok(stack.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::global;
    use crate::assert::Assert;

    #[test]
    fn test_simple() {
        let mut a = Assert::new();
        a.globals_add(global);
        a.is_true(
            r#"
def foo():
    return bar()

def bar():
    s = call_stack()
    return all([
        "foo()" in s,
        "bar()" in s,
        "call_stack()" in s,
    ])

foo()
            "#,
        );
    }

    #[test]
    fn test_strip_one() {
        let mut a = Assert::new();
        a.globals_add(global);
        a.is_true(
            r#"
def foo():
    return bar()

def bar():
    s = call_stack(strip_frames=1)
    return all([
        "foo()" in s,
        "bar()" in s,
        "call_stack()" not in s,
    ])

foo()
            "#,
        );
    }

    #[test]
    fn test_strip_all() {
        let mut a = Assert::new();
        a.globals_add(global);
        a.is_true(
            r#"
def foo():
    return bar()

def bar():
    s = call_stack(strip_frames=10)
    return not bool(s)

foo()
            "#,
        );
    }
}
