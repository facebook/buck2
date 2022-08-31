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

use crate::tests::bc::golden::bc_golden_test;

#[test]
fn test_return_type_erased() {
    // Return type is checked in bytecode.
    // In this example, return type is anything.
    // Check it is not present in bytecode.
    bc_golden_test("opt_return_type_erased", "def test() -> '_a': return 1");
}

#[test]
fn test_return_type_present() {
    // Self-check for previous test.
    bc_golden_test("opt_return_type_present", "def test() -> 'str': return 'x'");
}
