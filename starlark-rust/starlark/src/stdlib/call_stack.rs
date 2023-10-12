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
    fn call_stack(eval: &mut Evaluator) -> anyhow::Result<String> {
        let mut stack = eval.call_stack();
        // pop the call to call_stack() itself because that's not interesting
        stack.frames.pop();
        Ok(stack.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::global;
    use crate::assert::Assert;

    #[test]
    fn test_call_stack() {
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
        "call_stack()" not in s,
    ])

foo()
            "#,
        );
    }
}
