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

use crate::{eval::bc::opcode::BcOpcode, tests::bc::test_instrs};

#[test]
fn test_methods_invoked_speculatively() {
    test_instrs(
        &[BcOpcode::ReturnConst],
        r#"
def test():
    return "foo".startswith("f")
"#,
    )
}

#[test]
fn test_format_speculatively_before_format_instr() {
    test_instrs(
        &[BcOpcode::ReturnConst],
        r#"
def test():
    # Test this expression is compiled to constant, not to `FormatOne` instruction.
    return "x{}y".format(1)
"#,
    );
}
