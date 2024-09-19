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

use crate as starlark;
use crate::assert::Assert;
use crate::environment::GlobalsBuilder;
use crate::starlark_module;
use crate::values::none::NoneType;

#[starlark_module]
fn register_required_after_optional(globals: &mut GlobalsBuilder) {
    // This signature:
    // ```
    // def required_after_optional(opt=1, req): pass
    // ```
    // is not valid in Python. We should probably not allow it in native functions either.
    fn required_after_optional(opt: Option<i32>, req: String) -> anyhow::Result<NoneType> {
        let _ignore = (req, opt);
        Ok(NoneType)
    }
}

#[test]
fn test_both_by_name() {
    let mut a = Assert::new();
    a.globals_add(register_required_after_optional);
    a.pass("required_after_optional(req='a', opt=1)");
    a.pass("required_after_optional(opt=1, req='a')");
}

#[test]
fn test_both_pos() {
    let mut a = Assert::new();
    a.globals_add(register_required_after_optional);
    a.pass("required_after_optional(1, 'a')");
}

#[test]
fn test_req_by_name() {
    let mut a = Assert::new();
    a.globals_add(register_required_after_optional);
    a.pass("required_after_optional(req='a')");
}
