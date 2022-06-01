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

use crate::{assert, eval::bc::opcode::BcOpcode, tests::bc::test_instrs};

#[test]
fn test_type() {
    test_instrs(
        &[BcOpcode::Type, BcOpcode::Return],
        "def test(x): return type(x)",
    );
}

#[test]
fn test_percent_s_one() {
    test_instrs(
        &[BcOpcode::PercentSOne, BcOpcode::Return],
        "def test(x): return '((%s))' % x",
    )
}

#[test]
fn test_format_one() {
    test_instrs(
        &[BcOpcode::FormatOne, BcOpcode::Return],
        "def test(x): return '(({}))'.format(x)",
    )
}

#[test]
fn test_percent_s_one_format_one_eval() {
    assert::pass(
        r#"
load("assert.star", "assert")

def test(x):
    return ("<{}>".format(x), "<%s>" % x)

assert.eq(("<1>", "<1>"), test(1))
# Test format does not accidentally call `PercentSOne`.
assert.eq(("<(1,)>", "<1>"), test((1,)))
"#,
    );
}

#[test]
fn test_spec_exec_list() {
    // `list` function is const-evaluated and the resulting list is compiled as list instruction.
    test_instrs(
        &[BcOpcode::ListOfConsts, BcOpcode::Return],
        "def test(): return list((10, 20))",
    )
}

#[test]
fn test_call_maybe_known_method() {
    test_instrs(
        &[
            BcOpcode::Const,
            BcOpcode::CallMaybeKnownMethodPos,
            BcOpcode::ReturnConst,
        ],
        "def test(x): x.append(1)",
    );
}
