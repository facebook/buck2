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

use crate::{eval::bc::opcode::BcOpcode, tests::bc::test_instrs};

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
