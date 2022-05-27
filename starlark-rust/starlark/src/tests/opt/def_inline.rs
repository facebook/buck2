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

//! Test function bodies inlined.

use crate::{
    assert::Assert,
    eval::{bc::opcode::BcOpcode, compiler::def::FrozenDef},
    tests::bc::test_instrs,
    values::ValueLike,
};

#[test]
fn test_def_const_inlined() {
    test_instrs(
        &[BcOpcode::ReturnConst],
        r#"
def trivial():
    return 10

def test():
    return trivial()
"#,
    )
}

#[test]
fn test_def_list_inlined() {
    test_instrs(
        &[BcOpcode::ListOfConsts, BcOpcode::Return],
        r#"
def test():
    return returns_list()

# Also test function is inlined if it is defined after the caller.
def returns_list():
    return [10, True]
"#,
    )
}

#[test]
fn test_dict_inlined_call_stack() {
    let mut a = Assert::new();
    a.module("f.bzl", "def f(): return {[]: 10}");
    // For now inlining doesn't work within one module, so do different modules.
    let m_g = a.module("g.bzl", "load('f.bzl', 'f')\ndef g(): return f()");
    let m_h = a.module("h.bzl", "load('g.bzl', 'g')\ndef h(): return g()");

    // Check `f` is inlined into `g` and `h`.
    for (m, f) in [(m_g, "g"), (m_h, "h")] {
        let f = m.get(f).unwrap();
        let f = f.value().downcast_ref::<FrozenDef>().unwrap();
        assert_eq!(
            // TODO(nga): nothing is inlined here yet (so the opcode is `CallFrozenDefPos`),
            //   but it is inlined in the following diff, and test stays correct.
            BcOpcode::CallFrozenDefPos,
            f.bc().instrs.opcodes().as_slice()[0],
            "in `{}`",
            f,
        );
    }

    let error = a.fail(
        r"
load('h.bzl', 'h')
h()
",
        "",
    );

    assert_eq!(
        r"
Traceback (most recent call last):
  * assert.bzl:3, in <module>
    h()
  * h.bzl.bzl:2, in h
    def h(): return g()
  * g.bzl.bzl:2, in g
    def g(): return f()
error: Value of type `list` is not hashable
 --> f.bzl.bzl:1:18
  |
1 | def f(): return {[]: 10}
  |                  ^^
  |
",
        &format!("\n{:#}", error)
    );
}
